{-# LANGUAGE DeriveDataTypeable #-}
module Transient.EVars where

import Transient.Base
import Transient.Internals((!>),runTransState,onNothing, EventF(..), killChildren)
import qualified Data.Map as M
import Data.Typeable

import Control.Concurrent
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Exception(SomeException)

import Data.List(nub)
import Control.Monad.State

--newtype EVars= EVars  (IORef (M.Map Int [EventF]))  deriving Typeable

data EVar a= EVar Int (TVar (Int,Int))  (TChan (StreamData a)) deriving  Typeable


-- | creates an EVar.
--
-- Evars are event vars. `writeEVar` trigger the execution of all the continuations associated to the  `readEVar` of this variable
-- (the code that is after them).
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
-- Now the continuations are executed in parallel.
--
-- see https://www.fpcomplete.com/user/agocorona/publish-subscribe-variables-transient-effects-v
--

newEVar ::  TransientIO (EVar a)
newEVar  = Transient $ do
   id <- genId
   rn <- liftIO $ newTVarIO (0,0)
   ref <-liftIO  newTChanIO
   return . Just $ EVar id  rn ref

-- | delete al the subscriptions for an evar.
cleanEVar :: EVar a -> TransIO ()
cleanEVar (EVar id rn ref1)= liftIO $ atomically $ do
    writeTChan  ref1 SDone
    writeTVar rn (0,0)

-- | read the EVar. It only succeed when the EVar is being updated
-- The continuation gets registered to be executed whenever the variable is updated.
--
-- if readEVar is re-executed in any kind of loop, since each continuation is different, this will register
-- again. The effect is that the continuation will be executed multiple times
-- To avoid multiple registrations, use `cleanEVar`
--readEVar :: EVar a -> TransIO a
--readEVar (ev@(EVar id rn ref1))= do
--     onFinish $  const $ liftIO $ atomically $ do
--                        (n,n') <- readTVar rn
--                        writeTVar rn (n-1,n')
--     readEVar' ev

readEVar (EVar id rn ref1)= do
     liftIO $ atomically $ readTVar rn >>= \(n,n') -> writeTVar rn $ (n+1,n'+1)
     r <- parallel $ atomically $ do
                (n,n') <- readTVar rn

                if n'> 1 then do
                           r <- peekTChan ref1
                           writeTVar rn (n,n'-1)
                           return r
                         else do
                           r <- readTChan ref1
                           writeTVar rn (n,n)
                           return r

     case r of
        SDone -> empty
        SMore x -> return x
        SLast x -> return x
        SError e -> error $ "readEVar: "++ show e

-- |  update the EVar and execute all readEVar blocks with "last in-first out" priority
--
writeEVar (EVar id rn ref1) x= liftIO $ atomically $ do
       writeTChan  ref1 $ SMore x


-- | write the EVar and drop all the `readEVar` handlers.
--
-- It is like a combination of `writeEVar` and `cleanEVar`
lastWriteEVar (EVar id rn ref1) x= liftIO $ atomically $ do
       writeTChan  ref1 $ SLast x


-- Finalization


type FinishReason= Maybe SomeException

 | trigger finish when the stream data return SDone
checkFinalize v=
           case v of
              SDone ->  finish Nothing >> stop
              SLast x ->  return x
              SError e -> liftIO ( print e) >> finish Nothing >> stop
              SMore x -> return x



data Finish= Finish (EVar FinishReason) deriving Typeable

-- | initialize the event variable for finalization.
-- all the following computations in different threads will share it
initFinish :: TransIO Finish
initFinish= do
      fin <-  newEVar
      let f = Finish fin
      setData  f
      return  f


-- | suscribe a computation to be called when the finish event is triggered
onFinish :: (FinishReason ->TransIO ()) -> TransIO ()
onFinish  close=  do
       st <- get
       liftIO $ forkIO $ do  -- in another thread to be sure that it executes even when the current thread dies
                           runTransState st $ do
                               Finish finish <- getSData <|> initFinish
                               e <- readEVar finish
                               close e  -- !!> "CLOSE"
                               stop
                           return ()
       return ()




-- | trigger the event, so this closes all the resources
finish :: FinishReason -> TransIO ()
finish e= do
    liftIO $ putStr  "finish: " >> print e
    Finish finish <- getSData <|> initFinish
    lastWriteEVar finish e

-- | deregister all the finalization actions.
-- A initFinish is needed to register actions again
unFinish= do
    Finish fin <- getSData
    cleanEVar fin    -- !!> "DELEVAR"
   <|> return ()   -- !!> "NOT DELEVAR"


killOnFinish comp=   do
   return () !> "setup killonfinish"
   chs <- liftIO $ newTVarIO []
   onFinish $ const $ liftIO $ killChildren chs  !> "killOnFinish event"
   r <- comp
   modify $ \ s -> s{children= chs}
   return r
