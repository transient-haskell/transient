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

module Base3 where


import Control.Monad.State.Strict
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


data EventF  = forall a b . EventF{eventHandlers:: (M.Map String EventF )
                     ,currentEvent :: Maybe Event
                     ,xcomp :: (TransientIO a)
                     ,fcomp :: [a -> TransientIO b]}


eventf0= EventF M.empty Nothing (empty) [const $ empty]


instance MonadState EventF  TransientIO where
--  type StateType (Transient m)= EventF
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())

type StateIO= StateT EventF  IO

type TransientIO= Transient StateIO

runTrans ::  TransientIO x -> StateT EventF  IO (Maybe x)
runTrans (Transient mx) = mx

setEventCont ::   (TransientIO a) -> (a -> TransientIO b) -> StateIO EventF
setEventCont x f  = do
   st@(EventF es c x' fs)  <- get
   put $ EventF es c x ( f: unsafeCoerce fs) -- st{xcomp=  x, fcomp=  f: unsafeCoerce fs}
   return st

resetEventCont cont=do
      st <- get
      put cont {eventHandlers=eventHandlers st, currentEvent= currentEvent st}

getCont ::(MonadState EventF  m) => m EventF
getCont = get

runCont :: EventF -> StateIO ()
runCont (EventF _ _ x fs)= do runIt x (unsafeCoerce fs); return ()
   where
      runIt x fs= runTrans $  x >>= compose fs



      compose []= const empty
      compose (f: fs)= \x -> f x >>= compose fs


eventLoop :: [Event] ->  StateIO ()
eventLoop []= return()
eventLoop (ev@(Event name _):evs)= do
   (modify $ \st -> st{currentEvent= Just ev })  !> ("eventLoop:" ++ name)
   ths <- gets eventHandlers    !> "readMVar eventHandlers"
   case M.lookup name ths of
      Just st -> runCont st  !> "event Handler" 
      Nothing -> return ()   !> ("eventLoop: Nothing "++ show (M.size ths))
   eventLoop evs

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



--runTrans' :: TransientIO x -> IO (Maybe x)
--runTrans' tmx= do -- evalState (runTrans tmx) eventf0
--   st <- readIORef globalState
--   (x,st') <- runStateT (runTrans tmx)   eventf0 -- st
--   writeIORef globalState st'
--   return x


--runTrans'' ::  TransientIO a -> IO ()
--runTrans'' tmx= runTrans' tmx >> return ()


