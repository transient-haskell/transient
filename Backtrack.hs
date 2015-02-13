{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
module Backtrack (backRegister, onBacktrack, goBack, goForward, backCut) where

import Base
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Unsafe.Coerce

data Backtrack= forall a b.Backtrack{backtracking :: Bool
                                    ,backStack :: [EventF]}
                                    deriving Typeable

-- | assures that backtracking will not go further
backCut :: TransientIO ()
backCut= Transient $ do
     delSessionData $ Backtrack False []
     return $ Just ()

-- | the secod parameter will be executed when when backtracking 
onBacktrack ac bac= backRegister $ do 
    Backtrack back _ <- getSData <|> return (Backtrack False [])
    if back then bac else ac

-- | register an actions that will be executed when backtracking
backRegister :: TransientIO a -> TransientIO a
backRegister f  = Transient $ do
   st@(EventF   (i,x) fs d _  ro r)  <- get   !> "backregister"
   md  <- getSessionData
   setSessionData $   case md of
        Just (bss@(Backtrack b (bs@((EventF (i',_) _ _ _ _ _):_)))) -> if i== i' then bss else  Backtrack b $ unsafeCoerce(i,x,ro, fs):bs
        Nothing ->  Backtrack False [st]
   runTrans f

-- | restart the flow forward from this point on
goForward :: TransientIO ()
goForward= setSData $ Backtrack False []

-- | execute backtracking. It execute the registered actions in reverse order. 
--
-- If the backtracking flag is changed the flow proceed  forward from that point on. 
--
--If the backtrack stack is finished or backCut executed, `goBack` will stop.
goBack :: TransientIO ()
goBack= Transient $ do
  mv <- getSessionData                  !>"GOBACK"
  case mv of
    Just bs -> goBackt  bs
    Nothing -> return Nothing
  where
  goBackt (Backtrack _ [])= return Nothing                     !> "END"
  goBackt (Backtrack b (stack@((EventF(i,x) fs _ _ _ _): bs)))= do
        modify $ \cont -> cont{replay= True,mfSequence=i}
        setSData $ Backtrack True stack
        mr <-  runTrans x
        mb <- getSessionData
        case mb of
          Nothing -> return Nothing
          Just (Backtrack back _) -> do
             
             case back of
               True ->  goBackt $ Backtrack True bs            !> "BACK AGAIN"
               False -> case mr of
                   Nothing -> return empty                     !> "FORWARD END"
                   Just x -> do
                       runTrans $ compose (unsafeCoerce fs) x  !> "FORWARD EXEC"
                   
             return Nothing             
