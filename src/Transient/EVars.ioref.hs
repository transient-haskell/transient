{-# LANGUAGE DeriveDataTypeable #-}
module Transient.EVars where

import Transient.Base
import qualified Data.Map as M
import Data.Typeable

import Control.Concurrent
import Control.Applicative
import Control.Concurrent.MVar
import Data.IORef
import Control.Monad.State
import Data.List(nub)

newtype EVars= EVars  (IORef (M.Map Int [EventF]))  deriving Typeable

data EVar a= EVar Int  (IORef [a]) deriving Typeable


-- | creates an EVar.
--
-- Evars are event vars. `writeEVar` trigger the execution of all the continuations associated to the  `readEVar` of this variable
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
   getData `onNothing`  do -- initialize EVars
                            ref <- liftIO $ newIORef M.empty
                            setData $ EVars ref
                            return  (EVars ref)
   id <- genId

   ref <-liftIO $ newIORef []
   return . Just $ EVar id  ref

-- | delete al the subscriptions for an evar.
delEVar :: EVar a -> TransIO ()
delEVar (EVar id  _)= Transient $ do
   EVars ref <- getData `onNothing` error "No Events context"
   map <- liftIO $ readIORef ref
   liftIO $ writeIORef ref $ M.delete id map
   return $ Just ()

-- | read the EVar. It only succeed when the EVar is being updated
-- The continuation gets registered to be executed whenever the variable is updated.
-- if readEVar is in any kind of loop, since each continuation is different, this will register
-- again the continuation. The effect is that the continuation will be executed multiple times
-- To avoid multiple registrations, use `unsubscribe`
readEVar :: EVar a -> TransIO a
readEVar (EVar id  ref1)= Transient $ do
   mr <- liftIO $ readIORef ref1     !> "READEVAR"
   case mr of
     (c:_) ->  do return $ Just c   !> "being executed"
     [] -> do
         cont <- get                                    !> "SETTING THE EVAR"
         EVars ref <- getData `onNothing` error "No EVars context"
         map <- liftIO $ readIORef ref
         let Just conts=  M.lookup id map <|> Just []
         liftIO $ writeIORef ref $  M.insert id (cont:conts) map
         return Nothing

-- |  update the EVar and execute all readEVar blocks with "last in-first out" priority
writeEVar (EVar id  ref1) x= Transient $ do
   EVars revars <- getData `onNothing` error "No Events context"
   liftIO $ do
     (atomicModifyIORef' ref1 $ \xs -> (xs ++[x],()))           --  !> "writeEVar"
     loop revars
     return $ Just ()
   where
   loop revars= do
     map <- readIORef revars
     let Just conts = M.lookup id map <|> Just []
         len= length conts
     runCont'  len id revars
     rs <- (atomicModifyIORef' ref1 $ \xs -> (tail xs,tail xs))          !> "finish executing"
     if not $ null rs then loop revars else return $ Just ()




   runCont'  0 _ _ = return ()
   runCont'  n id revars= do
       map <- liftIO $ readIORef revars  !> "runCont'"
       let Just conts= M.lookup id map <|> Just []
       let current= head conts
           nexts= tail conts
--       let env = mfData current
--       modify $ \s-> s{mfData= env}     -- !> ("registered:", length conts)
--       runCont current                   !> "Run continuation event handler"
       runStateT (runCont current) current         !> "before"
       map' <- liftIO $ readIORef revars            !> "after"
       let Just conts'= M.lookup id map' <|> Just []
       if (length conts /= length conts')    -- to avoid infinite loops due to re-registrations
          then error "read of an EVar more than one time without using unsubscribe. Probably in code called repeatedly"
          else liftIO $ writeIORef revars $ M.insert id (nexts ++ [current]) map
       runCont'  (n - 1) id revars

-- | unsuscribe the last `readEVar` executed for this EVar
unsubscribe (EVar id  _)= Transient $ do
   EVars revars <- getData `onNothing` error "No Events context"
   map <- liftIO $ readIORef revars
   let Just conts = M.lookup id map <|> Just []
   liftIO $ writeIORef revars $  M.insert id (tail conts) map

   return $ Just ()


