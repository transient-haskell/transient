{-# LANGUAGE DeriveDataTypeable #-}
module Vars where

import Base
import Data.Map as M
import Data.Typeable
import Data.Maybe
import Control.Concurrent
import Control.Applicative
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.State

newtype EVars= EVars (M.Map Int [EventF]) deriving Typeable

data EVar a= EVar Int (IORef a)



newEVar :: a -> TransientIO (EVar a)
newEVar a = Transient $ do
   id <- genNewId
   ref <- liftIO $ newIORef a
   return . Just $ EVar id ref

readEVar (EVar id ref)= Transient $ do 
   cont <- getCont
   EVars map <- getSessionData `onNothing` return  (EVars M.empty) 
   let Just conts=  M.lookup  id map <|> Just []
   setSData . EVars $ M.insert id (cont:conts) map
   liftIO $ readIORef ref

writeEVar (EVar id ref) x= Transient $ do
   EVars map <- getSessionData `onNothing` return (EVars M.empty)
   liftIO $ writeIORef ref x
   let Just conts=   M.lookup  id map <|> Just []
   liftIO $ mapM_ (forkCont id Once (return())) conts
   return $ Just ()
 


    