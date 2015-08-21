{-# LANGUAGE DeriveDataTypeable #-}
module Transient.EVars where

import Transient.Base
import qualified Data.Map as M
import Data.Typeable

import Control.Concurrent
import Control.Applicative
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.State

newtype EVars= EVars  (IORef (M.Map Int [EventF]))  deriving Typeable

data EVar a= EVar Int (IORef (Maybe a))

-- * Evars are event vars. `writeEVar` trigger the execution of all the continuations associated to the  `readEVar` of this variable
-- (the code that is after them) as  stack: the most recent reads are executed first.
--
-- It is like the publish-subscribe pattern but without inversion of control, since a readEVar can be inserted at any place in the
-- Transient flow.
--
-- EVars are created upstream and can be used to communicate two sub-threads of the monad. Following the Transient philosophy they 
-- do not block his own thread if used with alternative operators, unlike the IORefs and TVars. And unlike STM vars, that are composable,
-- they wait for their respective events, while TVars execute the whole expression when any variable is modified.
-- 
-- The execution continues after the writeEVar when all subscribers have been executed.
--
-- see https://www.fpcomplete.com/user/agocorona/publish-subscribe-variables-transient-effects-v
-- 


newEVar ::  TransientIO (EVar a)
newEVar  = Transient $ do
   EVars ref <- getSessionData `onNothing`  do
                            ref <- liftIO $ newIORef M.empty
                            setSData $ EVars ref
                            return  (EVars ref) 
   id <- genNewId
   ref <- liftIO $ newIORef Nothing
   return . Just $ EVar id ref

readEVar :: EVar a -> TransIO a
readEVar (EVar id ref1)= Transient $ do 
   mr <- liftIO $ readIORef ref1
   case mr of
     Just _ -> return mr
     Nothing -> do
         cont <- getCont
         EVars ref <- getSessionData `onNothing` error "No Events context"
         map <- liftIO $ readIORef ref
         let Just conts=  M.lookup  id map <|> Just []
         liftIO $ writeIORef ref $  M.insert id (cont:conts) map
         return Nothing
         
writeEVar (EVar id ref1) x= Transient $ do
   EVars ref <- getSessionData `onNothing`  error "No Events context" 
   liftIO $ writeIORef ref1 $ Just x
   map <- liftIO $ readIORef ref
   let Just conts= M.lookup id map <|> Just []
   mapM runCont conts 
   liftIO $ writeIORef ref1 Nothing
   return $ Just ()
 



