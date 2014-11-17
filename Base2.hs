-----------------------------------------------------------------------------
--
-- Module      :  Base
-- Copyright   :
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification, FlexibleContexts,
             FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}

module Base2 where


import Control.Monad.State
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic
import Debug.Trace

(!>) = flip trace

data UDynamic= forall a.UDynamic a

cast1 (UDynamic x)= unsafeCoerce x

toUDyn= UDynamic

data Transient m x= Transient  (m (Maybe x))


data EventF  = EventF{eventHandlers:: (M.Map String EventF )
                     ,currentEvent :: Maybe Event
                     ,xcomp :: (StateIO(Maybe UDynamic))
                     ,fcomp ::  (UDynamic -> StateIO (Maybe UDynamic))}


eventf0= EventF M.empty Nothing (return Nothing) (const $ return Nothing)


instance MonadState EventF TransientIO where
--  type StateType (Transient m)= EventF
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())


type StateIO= StateT EventF IO

type TransientIO= Transient StateIO

runTrans ::  TransientIO x -> StateT EventF IO (Maybe x)
runTrans (Transient mx) = mx

setEventCont ::   (TransientIO a) -> (a -> TransientIO b) -> StateIO EventF
setEventCont x f = do
   st <- get
   modify $ \st -> st{xcomp=  (dynamize1  x)  ,fcomp=  dynamize f  `addto` fcomp st}
   return st

dynamize1 x= runTrans x >>= return . fmap toUDyn

dynamize ::  (a -> TransientIO b) => UDynamic -> StateIO(Maybe UDynamic)
dynamize   f= \d -> do
         my <- runTrans $ f $ cast1 d
         return $ fmap toUDyn my

addto :: (UDynamic -> StateIO (Maybe UDynamic))   -> (UDynamic -> StateIO (Maybe UDynamic)) -> (UDynamic -> StateIO (Maybe UDynamic))
addto f f'= \x -> do
         mr <-  f x   !> "f x"
         case mr of
           Nothing -> return Nothing  !> "not FOLLOW"
           Just x' -> (f' x') !> "FOLLOW"

resetEventCont cont=
   modify $ \st -> st{xcomp= xcomp cont, fcomp= fcomp cont}


eventLoop :: [Event] ->  StateIO ()
eventLoop []= return()
eventLoop (ev@(Event name _):evs)= do
   (modify $ \st -> st{currentEvent=Just ev })  !> ("eventLoop:" ++ name)
   ths <- gets eventHandlers    !> "readMVar eventHandlers"
   case M.lookup name ths of
      Just (EventF _ _ x fs) -> (( x `addto` fs) >> return ())
             !> "event Handler"
      Nothing -> return () !> ("eventLoop: Nothing")
   eventLoop evs
   where
   addto f f'= do
         mr <-  f 
         case mr of
           Nothing -> return Nothing  !> "addto' not follow"
           Just x' -> (f' x') !> "addto' follow"

instance   Functor TransientIO where
  fmap f x= Transient $ fmap (fmap f) $ runTrans x


instance Applicative TransientIO where
  pure a  = Transient  .  return $ Just a
  Transient f <*> Transient g= Transient $
       f >>= \ k ->
       g >>= \ x ->
       return $  k <*> x

instance  Alternative TransientIO where
  empty= Transient $ return  Nothing
  Transient f <|> Transient g= Transient $ do
       x <- f
       y <- g
       return $ x <|> y


instance Monad TransientIO where
      return x = Transient $ return $ Just x
      x >>= f = Transient $ do
        cont <- setEventCont x  f
        mk <- runTrans x
        resetEventCont cont
        case mk of
           Just k  ->  runTrans $ f k
           Nothing -> return Nothing

instance MonadTrans (Transient ) where
  lift mx = Transient $ mx >>= return . Just

instance MonadIO TransientIO where
  liftIO = lift . liftIO --     let x= liftIO io in x `seq` lift x



type EvType = String
data Event = Event EvType Dynamic



runTrans' :: TransientIO x -> IO (Maybe x)
runTrans' tmx= do -- evalState (runTrans tmx) eventf0
--   st <- readIORef globalState
   (x,st') <- runStateT (runTrans tmx)   eventf0 -- st
--   writeIORef globalState st'
   return x


runTrans'' ::  TransientIO a -> IO ()
runTrans'' tmx= runTrans' tmx >> return ()


