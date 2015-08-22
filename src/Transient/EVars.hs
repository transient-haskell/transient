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
import Data.List(nub)

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

-- | creates an EVar
newEVar ::  TransientIO (EVar a)
newEVar  = Transient $ do
   getSessionData `onNothing`  do -- initialize EVars
                            ref <- liftIO $ newIORef M.empty
                            setSData $ EVars ref
                            return  (EVars ref)
   id <- genNewId
   ref <- liftIO $ newIORef Nothing
   return . Just $ EVar id ref


-- | read the EVar. It only succeed when the EVar is being updated
-- The continuation gets registered to be executed whenever the variable is updated.
-- if readEVar is in any kind of loop, since each continuation is different, this will register
-- again the continuation. The effect is that the continuation will be executed multiple times
-- To avoid multiple registrations, use `unsubscribe`
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

-- |  update the EVar and execute all readEVar blocks with last in - first out priority
writeEVar (EVar id ref1) x= Transient $ do
   EVars ref <- getSessionData `onNothing` error "No Events context"
   liftIO $ writeIORef ref1 $ Just x
   map <- liftIO $ readIORef ref
   let Just conts = M.lookup id map <|> Just []
       len= length conts
   runCont'  len id ref 
   liftIO $ writeIORef ref1 Nothing
   return $ Just ()
   
   where
   runCont'  0 _ _ = return () 
   runCont'  n id ref= do
       map <- liftIO $ readIORef ref
       let Just conts= M.lookup id map <|> Just []
       let current= head conts
           nexts= tail conts
       runCont current
       map' <- liftIO $ readIORef ref
       let Just conts'= M.lookup id map' <|> Just []
       if (length conts /= length conts') then return () else liftIO $ writeIORef ref $   M.insert id (nexts ++ [current]) map 
       runCont'  (n - 1) id ref

-- | unsuscribe the last `readEVar` executed for this EVar
unsubscribe (EVar id _)= Transient $ do
   EVars ref <- getSessionData `onNothing` error "No Events context"
   map <- liftIO $ readIORef ref
   let Just conts = M.lookup id map <|> Just []
   liftIO $ writeIORef ref $  M.insert id (tail conts) map
   
   return $ Just ()

   