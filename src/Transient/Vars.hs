{-# LANGUAGE DeriveDataTypeable #-}
module Transient.Vars where

import Base
import Data.Map as M
import Data.Typeable

import Control.Concurrent
import Control.Applicative
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.State

newtype EVars= EVars (M.Map Int [EventF]) deriving Typeable

data EVar a= EVar Int (IORef a)

-- * concurrency effect 
-- Evars are event vars. `readEVar` trigger the continuation (the code that is after them) when the var is updated with writeEVar
-- It is like the publish-suscribe pattern but without inversion of control, since a readEVar can be inserted at any place in the
-- Transient flow.
--
-- EVars are created upstream and can be used to communicate two subbranches of the monad. Following the Transient philosophy they 
-- do not block his own thread, unlike MVars or TVar's `retry`. They have a semantic similar to TVars when combined with `parallel`, so they are 
-- experimental, expecting some advantage of this model over TVar+ parallel
-- (not tested yet)

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
 



