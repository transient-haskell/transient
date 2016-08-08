{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | <https://www.fpcomplete.com/user/agocorona/the-hardworking-programmer-ii-practical-backtracking-to-undo-actions>

module Transient.Backtrack (onUndo, undo, retry, undoCut,registerUndo,

-- * generalized versions of backtracking with an extra parameter that gives the reason for going back
-- different kinds of backtracking with different reasons can be managed in the same program
onBack, back, backCut,registerBack,

-- * finalization primitives
finish, onFinish, initFinish,killOnFinish,checkFinalize, FinishReason
) where

import Transient.Base
import Transient.Internals(EventF(..),killChildren,onNothing,runClosure,runContinuation)
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Unsafe.Coerce
import System.Mem.StableName
import Control.Exception
import Control.Concurrent.STM hiding (retry)

data Backtrack b= Backtrack{backtracking :: Bool
                           ,backStack :: [EventF]
                           ,backReason :: b}
                                    deriving Typeable

-- | assures that backtracking will not go further back
backCut :: Typeable reason => reason -> TransientIO ()
backCut reason= Transient $ do
     delData $ Backtrack False [] reason
     return $ Just ()

undoCut ::  TransientIO ()
undoCut = backCut ()

-- | the second parameter will be executed when backtracking
{-# NOINLINE onUndo #-}
onBack :: Typeable b => TransientIO a -> b -> TransientIO a -> TransientIO a
onBack ac reason bac= registerBack reason $ Transient $ do
     Backtrack back _ _ <- getData `onNothing` return (Backtrack False [] reason)
     runTrans $ if back then bac  else ac

onUndo ::  TransientIO a -> TransientIO a -> TransientIO a
onUndo x y= onBack x () y


-- | register an action that will be executed when backtracking
{-# NOINLINE registerUndo #-}
registerBack :: Typeable b => b -> TransientIO a -> TransientIO a
registerBack reason f  = Transient $ do
   cont@(EventF _ _ x _ _ _ _ _ _ _ _)  <- get   -- !!> "backregister"

   md  <- getData `asTypeOf` (Just <$> backStateOf reason)

   ss <- case md of
        Just (bss@(Backtrack b (bs@((EventF _ _ x'  _ _ _ _ _ _ _ _):_))_)) -> do
            addrx  <- addr x
            addrx' <- addr x'         -- to avoid duplicate backtracking points
            return $ if addrx == addrx' then bss else  Backtrack b (cont:bs) reason
        Nothing ->  return $ Backtrack False [cont] reason
   setData ss
   runTrans f
   where
   addr x = liftIO $ return . hashStableName =<< (makeStableName $! x)


registerUndo :: TransientIO a -> TransientIO a
registerUndo f= registerBack () f

-- | restart the flow forward from this point on
retry :: Typeable b => b -> TransIO ()
retry reason= Transient $ do
    Backtrack _ stack _<- getData `onNothing` return (Backtrack False [] reason)
    setData $ Backtrack False stack reason
    return $ Just ()

-- | execute backtracking. It execute the registered actions in reverse order.
--
-- If the backtracking flag is changed the flow proceed  forward from that point on.
--
--If the backtrack stack is finished or undoCut executed, `undo` will stop.
back :: Typeable b => b -> TransientIO a
back reason = Transient $ do
  bs <- getData  `onNothing` return nullBack            -- !!>"GOBACK"
  goBackt  bs

  where
  nullBack= Backtrack False [] reason
  goBackt (Backtrack _ [] _)= return Nothing                      -- !!> "END"
  goBackt (Backtrack b (stack@(first@(EventF _ _ x fs _ _  _ _ _ _ _): bs)) _)= do

        setData $ Backtrack True stack reason
        mr <-  runClosure first                                 -- !!> "RUNCLOSURE"
        Backtrack back _ _ <- getData `onNothing` return nullBack `asTypeOf` backStateOf reason
                                                                -- !!>"END RUNCLOSURE"
        case back of
           True ->  goBackt $ Backtrack True bs reason                 -- !!> "BACK AGAIN"
           False -> case mr of
                   Nothing -> return empty                      -- !!> "FORWARD END"
                   Just x ->  runContinuation first x           -- !!> "FORWARD EXEC"

backStateOf reason= return  $ Backtrack False [] reason

undo ::  TransientIO a
undo= back ()

------ finalization

type FinishReason= Maybe SomeException

-- | initialize the event variable for finalization.
-- all the following computations in different threads will share it
-- it also isolate this event from other branches that may have his own finish variable
initFinish= backCut (Nothing :: FinishReason)

-- | set a computation to be called when the finish event happens
onFinish :: (FinishReason ->TransIO ()) -> TransIO ()
onFinish f= (return ()) `onBack` (Nothing :: FinishReason) $ do
    Backtrack back _ reason <- getData `onNothing`  backStateOf (Nothing :: Maybe SomeException)
    f reason

-- | trigger the event, so this closes all the resources
finish :: FinishReason -> TransIO ()
finish reason= back reason





-- | kill all the processes generated by the parameter when finish event occurs
killOnFinish comp= do

   chs <- liftIO $ newTVarIO []
   onFinish $ const $ liftIO $ killChildren chs   -- !> "killOnFinish event"
   r <- comp
   modify $ \ s -> s{children= chs}
   return r

-- | trigger finish when the stream of data ends
checkFinalize v=
           case v of
              SDone ->  finish Nothing >> stop
              SLast x ->  return x
              SError e -> liftIO ( print e) >> finish Nothing >> stop
              SMore x -> return x
