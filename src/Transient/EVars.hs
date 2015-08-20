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

newtype EVars= EVars  (IORef (M.Map Int [EventF])) deriving Typeable

data EVar a= EVar Int (IORef (Maybe a))

-- * concurrency effect 
-- Evars are event vars. `writeEVar` trigger the execution of all the continuations associated to the  `readEVar` of this variable
-- (the code that is after them) as  stack: the most recent reads are executed first.
--
-- It is like the publish-suscribe pattern but without inversion of control, since a readEVar can be inserted at any place in the
-- Transient flow.
--
-- EVars are created upstream and can be used to communicate two subbranches of the monad. Following the Transient philosophy they 
-- do not block his own thread if used with alternative operators, unlike the IO and STM vars. 
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
         cont <- getCont
         EVars ref <- getSessionData `onNothing` error "No Events context"
         map <- liftIO $ readIORef ref
         let Just conts=  M.lookup  id map <|> Just []
         liftIO $ writeIORef ref $  M.insert id (cont:conts) map
         return Nothing
         liftIO $ readIORef ref1
         
writeEVar (EVar id ref1) x= Transient $ do
   EVars ref <- getSessionData `onNothing`  error "No Events context" 
   liftIO $ writeIORef ref1 $ Just x
   map <- liftIO $ readIORef ref
   let Just conts= M.lookup id map <|> Just []
   mapM runCont conts 
   return $ Just ()
 



