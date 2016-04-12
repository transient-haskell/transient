{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | <https://www.fpcomplete.com/user/agocorona/the-hardworking-programmer-ii-practical-backtracking-to-undo-actions>

module Transient.Backtrack (registerUndo, onUndo, undo, retry, undoCut) where

import Transient.Base
import Transient.Internals(EventF(..),onNothing,runClosure,runContinuation)
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Unsafe.Coerce
import System.Mem.StableName

data Backtrack= forall a b.Backtrack{backtracking :: Bool
                                    ,backStack :: [EventF]}
                                    deriving Typeable

-- | assures that backtracking will not go further back
undoCut :: TransientIO ()
undoCut= Transient $ do
     delSData $ Backtrack False []
     return $ Just ()

-- | the secod parameter will be executed when backtracking
{-# NOINLINE onUndo #-}
onUndo :: TransientIO a -> TransientIO a -> TransientIO a
onUndo ac bac= registerUndo $ Transient $ do
     Backtrack back _ <- getData `onNothing` return (Backtrack False [])
     runTrans $ if back then bac  else ac


-- | register an action that will be executed when backtracking
{-# NOINLINE registerUndo #-}
registerUndo :: TransientIO a -> TransientIO a
registerUndo f  = Transient $ do
   cont@(EventF _ _ x _ _ _ _ _ _ _ _)  <- get   -- !!> "backregister"
   md  <- getData
   ss <- case md of
        Just (bss@(Backtrack b (bs@((EventF _ _ x'  _ _ _ _ _ _ _ _):_)))) -> do
            addrx  <- addr x
            addrx' <- addr x'         -- to avoid duplicate backtracking points
            return $ if addrx == addrx' then bss else  Backtrack b $ cont:bs
        Nothing ->  return $ Backtrack False [cont]
   setData ss
   runTrans f
   where
   addr x = liftIO $ return . hashStableName =<< (makeStableName $! x)

-- | restart the flow forward from this point on
retry :: TransIO ()
retry= Transient $ do
    Backtrack _ stack <- getData `onNothing` return (Backtrack False [])
    setData $ Backtrack False stack
    return $ Just ()

-- | execute backtracking. It execute the registered actions in reverse order.
--
-- If the backtracking flag is changed the flow proceed  forward from that point on.
--
--If the backtrack stack is finished or undoCut executed, `undo` will stop.
undo :: TransientIO a
undo= Transient $ do
  bs <- getData  `onNothing` return nullBack            -- !!>"GOBACK"
  goBackt  bs

  where
  nullBack= Backtrack False []
  goBackt (Backtrack _ [])= return Nothing                      -- !!> "END"
  goBackt (Backtrack b (stack@(first@(EventF _ _ x fs _ _  _ _ _ _ _): bs)))= do
--        put first{replay=True}
        setData $ Backtrack True stack
        mr <-  runClosure first                                 -- !!> "RUNCLOSURE"
        Backtrack back _ <- getData `onNothing` return nullBack
                                                                -- !!>"END RUNCLOSURE"
        case back of
           True ->  goBackt $ Backtrack True bs                 -- !!> "BACK AGAIN"
           False -> case mr of
                   Nothing -> return empty                      -- !!> "FORWARD END"
                   Just x ->  runContinuation first x           -- !!> "FORWARD EXEC"

