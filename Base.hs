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

module Base where


import Control.Monad.State
import Data.IORef
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic
import Debug.Trace

(!>) = flip trace

data UDynamic= forall a.UDynamic a

cast1 (UDynamic x)= Just $ unsafeCoerce x

toUDyn= UDynamic

data Transient m x= Transient  (m (Maybe x))

data EventF  = EventF  (IO(Maybe UDynamic))  (UDynamic -> IO (Maybe UDynamic))

globalState= unsafePerformIO $ newIORef eventf0

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
   env@(EventF _ fs) <- get
   put $ EventF  (dynamize1  x)  $  dynamize f `addto` fs
   return env

dynamize1 x= runTrans' x >>= return . fmap toUDyn

dynamize ::  (a -> TransientIO b) => UDynamic -> IO(Maybe UDynamic)
dynamize   f= \d -> do
      case cast1 d of
       Nothing -> return Nothing
       Just x -> do
         my <- runTrans' $ f x
         return $ fmap toUDyn my

addto :: (UDynamic -> IO (Maybe UDynamic))   -> (UDynamic -> IO (Maybe UDynamic)) -> (UDynamic -> IO (Maybe UDynamic))
addto f f'= \x -> do
         mr <-  f x
         case mr of
           Nothing -> return Nothing
           Just x' -> (f' x')

resetEventCont cont= put cont

currentEvent :: IORef  (Maybe Event)
currentEvent= unsafePerformIO $ newIORef Nothing

eventLoop :: [Event] ->  IO ()
eventLoop []= return()
eventLoop (ev@(Event name _):evs)= do
   writeIORef  currentEvent $ Just ev   !> ("eventLoop:" ++ name)
   ths <- readIORef eventHandlers    !> "readMVar eventHandlers"
   case M.lookup name ths of
      Just (EventF x fs) -> ((const x) `addto` fs) (error "eventLoop: undefined") >> return ()
             !> "event Handler"
      Nothing -> return () !> ("eventLoop: Nothing")
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



eventHandlers :: IORef (M.Map String EventF )
eventHandlers= unsafePerformIO $ newIORef M.empty

type EvType = String
data Event = Event EvType Dynamic



runTrans' :: TransientIO x -> IO (Maybe x)
runTrans' tmx= do -- evalState (runTrans tmx) eventf0
--   st <- readIORef globalState
   (x,st') <- runStateT (runTrans tmx)   eventf0 -- st
--   writeIORef globalState st'
   return x

eventf0= EventF (return Nothing) (const $ return Nothing)


runTrans'' ::  TransientIO a -> IO ()
runTrans'' tmx= runTrans' tmx >> return ()


