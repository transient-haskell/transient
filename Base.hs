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
{-# LANGUAGE ExistentialQuantification,FlexibleContexts,
             FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}

module Base where


import Control.Monad.State.Strict
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Map as M
import Data.Dynamic
import Debug.Trace

(!>) = const . id -- flip trace


data Transient m x= Transient  (m (Maybe x))


data EventF  = forall a b . EventF{eventHandlers:: (M.Map String EventF )
                                  ,currentEvent :: Maybe Event
                                  ,eventValues :: M.Map String Dynamic
                                  ,xcomp :: (TransientIO a)
                                  ,fcomp :: [a -> TransientIO b]}


eventf0= EventF M.empty Nothing M.empty (empty) [const $ empty]


instance MonadState EventF  TransientIO where
  get=  Transient $ get >>= return . Just
  put x= Transient $ put x >> return (Just ())

type StateIO= StateT EventF  IO

type TransientIO= Transient StateIO

runTrans ::  TransientIO x -> StateT EventF  IO (Maybe x)
runTrans (Transient mx) = mx

setEventCont ::   (TransientIO a) -> (a -> TransientIO b) -> StateIO EventF
setEventCont x f  = do
   st@(EventF es c vs x' fs)  <- get
   put $ EventF es c vs  x ( f: unsafeCoerce fs) -- st{xcomp=  x, fcomp=  f: unsafeCoerce fs}
   return st

resetEventCont cont=do
      st <- get
      put cont {eventHandlers=eventHandlers st, eventValues=eventValues st, currentEvent= currentEvent st}

getCont ::(MonadState EventF  m) => m EventF
getCont = get

runCont :: EventF -> StateIO ()
runCont (EventF _ _ _ x fs)= do runIt x (unsafeCoerce fs); return ()
   where
      runIt x fs= runTrans $  x >>= compose fs



      compose []= const empty
      compose (f: fs)= \x -> f x >>= compose fs


eventLoop :: [Event] ->  StateIO ()
eventLoop []= return()
eventLoop (ev@(Event name _):evs)= do
   (modify $ \st -> st{currentEvent= Just ev ,eventValues= M.delete name $ eventValues st})  !> ("inject event:" ++ name)
   ths <- gets eventHandlers
   case M.lookup name ths of
      Just st -> runCont st  !> ("execute event handler for: "++ name) 
      Nothing -> return ()   !> "no handler for the event"
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
           Just k  -> runTrans $ f k
           Nothing -> return Nothing

instance MonadTrans (Transient ) where
  lift mx = Transient $ mx >>= return . Just

instance MonadIO TransientIO where
  liftIO = lift . liftIO --     let x= liftIO io in x `seq` lift x



type EvType = String
data Event = Event EvType Dynamic



currentEventValue :: Typeable a => String -> TransientIO a
currentEventValue name =  do
  st <- get !> "currValue"
  let vals= eventValues st
  case M.lookup name vals of
      Nothing -> waitEvent name
      Just v  -> return $ fromDyn v (error "currentEventValue: type error") 
      
  
waitEvent :: Typeable a => String -> TransientIO a
waitEvent name = Transient $ do
  st <- get !> "waitEvent"
  let evs = eventHandlers  st 

  case  M.lookup name evs of
    Nothing ->  do
       put st{ eventHandlers=  M.insert name st  evs} !> ("created event handler for: "++ name)
       return Nothing 
    Just _ ->  do
       put st{ eventHandlers=  M.insert name st evs} !> ("upadated event handler for: "++ name)
       eventValue name

eventValue name =  do
   st <- get
   let me= currentEvent st 
   case me of
     Nothing -> return Nothing   !> "NO current EVENT"
     Just (Event name' r) -> do
      if name /= name' then return Nothing  else do
        case fromDynamic r of
          Nothing -> return Nothing 
          Just x -> do 
            liftIO $ putStrLn $ "read event: " ++ name
            put st{eventValues= M.insert name (toDyn x) $ eventValues st}
            return $ Just x
