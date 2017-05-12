{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Transient allows you to pair an action with an undo action. As actions are
-- executed the corresponding undo actions are saved. At any point an 'undo'
-- can be triggered which executes all the undo actions registered till now in
-- reverse order. An undo action can decide to reverse the execution by using
-- 'retry', forward execution will resume from that point on. Multiple
-- independent undo tracks can be defined. There is an implicit default track
-- for common case.
--
-- See
-- <https://www.fpcomplete.com/user/agocorona/the-hardworking-programmer-ii-practical-backtracking-to-undo-actions this blog post>
-- for more details.

module Transient.Backtrack (

-- * Multi-track Undo
-- $multitrack
onBack, back, forward, backCut,

-- * Default Track Undo
-- $defaulttrack
onUndo, undo, retry, undoCut,

-- * Finalization Primitives
finish, onFinish, onFinish' ,initFinish , noFinish,checkFinalize , FinishReason
) where

import Transient.Internals

import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Unsafe.Coerce
import System.Mem.StableName
import Control.Exception
import Control.Concurrent.STM hiding (retry)
import Data.Maybe

-- $defaulttrack
--
-- Simpler APIs for the common case using an implicit default undo track @()@.
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Monad.IO.Class (liftIO)
-- import Transient.Base (keep)
-- import Transient.Backtrack (onUndo, undo, retry)
--
-- main = keep $ do
--     step 1 >> tryAgain >> step 2 >> step 3 >> undo >> return ()
--     where
--         step n = liftIO (putStrLn ("Do Step: " ++ show n))
--                  `onUndo`
--                  liftIO (putStrLn ("Undo Step: " ++ show n))
--
--         tryAgain = liftIO (putStrLn "Will retry on undo")
--                    `onUndo`
--                    (retry >> liftIO (threadDelay 1000000 >> putStrLn "Retrying..."))
-- @

-- $multitrack
--
-- Track-specific undo primitives. An undo track is identified by a
-- data type. The data type of each distinct track must be different.
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Monad.IO.Class (liftIO)
-- import Transient.Base (keep)
-- import Transient.Backtrack (onBack, forward, back)
--
-- data TrackX = TrackX deriving Show
--
-- main = keep $ do
--     step 1 >> goForward >> step 2 >> step 3 >> back TrackX >> return ()
--     where
--           step n = liftIO (putStrLn ("Execute Step: " ++ show n))
--                    `onBack`
--                    (\TrackX -> liftIO (putStrLn ("Undo Step: " ++ show n)))
--
--           goForward = liftIO (putStrLn "Turning point")
--                       `onBack` (\TrackX ->
--                                     (forward TrackX
--                                      >> liftIO (threadDelay 1000000
--                                                 >> putStrLn "Going forward..."))
--                                )
-- @

--
--data Backtrack b= Show b =>Backtrack{backtracking :: Maybe b
--                                    ,backStack :: [EventF] }
--                                    deriving Typeable
--
--
--
---- | assures that backtracking will not go further back
--backCut :: (Typeable reason, Show reason) => reason -> TransientIO ()
--backCut reason= Transient $ do
--     delData $ Backtrack (Just reason)  []
--     return $ Just ()
--
--undoCut ::  TransientIO ()
--undoCut = backCut ()
--
---- | the second parameter will be executed when backtracking
--{-# NOINLINE onBack #-}
--onBack :: (Typeable b, Show b) => TransientIO a -> ( b -> TransientIO a) -> TransientIO a
--onBack ac  bac= registerBack (typeof bac) $ Transient $ do
--     Backtrack mreason _  <- getData `onNothing` backStateOf (typeof bac)
--     runTrans $ case mreason of
--                  Nothing     -> ac
--                  Just reason -> bac reason
--     where
--     typeof :: (b -> TransIO a) -> b
--     typeof = undefined
--
--onUndo ::  TransientIO a -> TransientIO a -> TransientIO a
--onUndo x y= onBack x (\() -> y)
--
--
---- | register an action that will be executed when backtracking
--{-# NOINLINE registerUndo #-}
--registerBack :: (Typeable b, Show b) => b -> TransientIO a -> TransientIO a
--registerBack witness f  = Transient $ do
--   cont@(EventF _ _ x _ _ _ _ _ _ _ _)  <- get   -- !!> "backregister"
--
--   md <- getData `asTypeOf` (Just <$> backStateOf witness)
--
--   case md of
--            Just (bss@(Backtrack b (bs@((EventF _ _ x'  _ _ _ _ _ _ _ _):_)))) ->
--               when (isNothing b) $ do
--                   addrx  <- addr x
--                   addrx' <- addr x'         -- to avoid duplicate backtracking points
--                   setData $ if addrx == addrx' then bss else  Backtrack mwit (cont:bs)
--            Nothing ->  setData $ Backtrack mwit [cont]
--
--   runTrans f
--   where
--   mwit= Nothing `asTypeOf` (Just witness)
--   addr x = liftIO $ return . hashStableName =<< (makeStableName $! x)
--
--
--registerUndo :: TransientIO a -> TransientIO a
--registerUndo f= registerBack ()  f
--
---- | restart the flow forward from this point on
--forward :: (Typeable b, Show b) => b -> TransIO ()
--forward reason= Transient $ do
--    Backtrack _ stack <- getData `onNothing`  (backStateOf reason)
--    setData $ Backtrack(Nothing `asTypeOf` Just reason)  stack
--    return $ Just ()
--
--retry= forward ()
--
--noFinish= forward (FinishReason Nothing)
--
---- | execute backtracking. It execute the registered actions in reverse order.
----
---- If the backtracking flag is changed the flow proceed  forward from that point on.
----
---- If the backtrack stack is finished or undoCut executed, `undo` will stop.
--back :: (Typeable b, Show b) => b -> TransientIO a
--back reason = Transient $ do
--  bs <- getData  `onNothing`  backStateOf  reason           -- !!>"GOBACK"
--  goBackt  bs
--
--  where
--
--  goBackt (Backtrack _ [] )= return Nothing                      -- !!> "END"
--  goBackt (Backtrack b (stack@(first : bs)) )= do
--        (setData $ Backtrack (Just reason) stack)
--
--        mr <-  runClosure first                                  -- !> "RUNCLOSURE"
--
--        Backtrack back _ <- getData `onNothing`  backStateOf  reason
--                                                                 -- !> "END RUNCLOSURE"
--        case back of
--           Nothing -> case mr of
--                   Nothing ->  return empty                      -- !> "FORWARD END"
--                   Just x  ->  runContinuation first x           -- !> "FORWARD EXEC"
--           justreason -> goBackt $ Backtrack justreason bs       -- !> ("BACK AGAIN",back)
--
--backStateOf :: (Monad m, Show a, Typeable a) => a -> m (Backtrack a)
--backStateOf reason= return $ Backtrack (Nothing `asTypeOf` (Just reason)) []
--
--undo ::  TransIO a
--undo= back ()
--
-------- finalization
--
--newtype FinishReason= FinishReason (Maybe SomeException) deriving (Typeable, Show)
--
---- | initialize the event variable for finalization.
---- all the following computations in different threads will share it
---- it also isolate this event from other branches that may have his own finish variable
--initFinish= backCut (FinishReason Nothing)
--
---- | set a computation to be called when the finish event happens
--onFinish :: ((Maybe SomeException) ->TransIO ()) -> TransIO ()
--onFinish f= onFinish' (return ()) f
--
--
---- | set a computation to be called when the finish event happens this only apply for
--onFinish' ::TransIO a ->((Maybe SomeException) ->TransIO a) -> TransIO a
--onFinish' proc f= proc `onBack`   \(FinishReason reason) ->
--    f reason
--
--
---- | trigger the event, so this closes all the resources
--finish :: Maybe SomeException -> TransIO a
--finish reason= back (FinishReason reason)
--
--
---- | kill all the processes generated by the parameter when finish event occurs
--killOnFinish comp= do
--   chs <- liftIO $ newTVarIO []
--   onFinish $ const $ liftIO $ killChildren chs   -- !> "killOnFinish event"
--   r <- comp
--   modify $ \ s -> s{children= chs}
--   return r
--
---- | trigger finish when the stream of data ends
--checkFinalize v=
--           case v of
--              SDone ->  finish Nothing >> stop
--              SLast x ->  return x
--              SError e -> liftIO ( print e) >> finish  Nothing >> stop
--              SMore x -> return x
