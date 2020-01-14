{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Transient implements an event handling mechanism ("backtracking") which
-- allows registration of one or more event handlers to be executed when an
-- event occurs. This common underlying mechanism called is used to handle
-- three different types of events:
--
-- * User initiated actions to run undo and retry actions on failures
-- * Finalization actions to run at the end of a task
-- * Exception handlers to run when exceptions are raised
--
-- Backtracking works seamlessly across thread boundaries.  The freedom to put
-- the undo, exception handling and finalization code where we want it allows
-- us to write modular and composable code.
--
-- Note that backtracking (undo, finalization or exception handling) does not
-- roll back the user defined state in any way. It only
-- executes the user-defined handlers. State changes are only caused via user
-- defined actions. These actions also can change the state as it was when backtracking started.
--
-- This example prints the final state as "world".
--
-- @
-- import Transient.Base (keep, setState, getState)
-- import Transient.Backtrack (onUndo, undo)
-- import Control.Monad.IO.Class (liftIO)
--
-- main = keep $ do
--     setState "hello"
--     oldState <- getState
--
--     liftIO (putStrLn "Register undo") \`onUndo\` (do
--         curState <- getState
--         liftIO $ putStrLn $ "Final state: "  ++ curState
--         liftIO $ putStrLn $ "Old state: "    ++ oldState)
--
--     setState "world" >> undo >> return ()
-- @
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
-- $finalization
onFinish, onFinish', finish, noFinish, initFinish
) where

import Transient.Internals


-- Code moved to Internals in order to manage exceptions in spawned threads.
