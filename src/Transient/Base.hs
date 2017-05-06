{-# LANGUAGE ScopedTypeVariables #-}
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
-- | See the
-- <https://github.com/transient-haskell/transient/wiki/Transient-tutorial tutorial>
-- for an overview of concepts and examples.  Also see the
-- <http://github.com/agocorona/transient readme> on the github repository.
-----------------------------------------------------------------------------

module Transient.Base(
-- * The Monad
TransIO(..), TransientIO
-- * Running the monad
,keep, keep', stop, exit

-- * Asynchronous standard input
,option, input

-- * Asynchronous IO operations
,async,waitEvents, spawn, parallel, sample
,react

-- * State management
,setState, setData, getState, getSData,getData,delState,delData, modifyData,modifyState,try

-- * Thread management
, threads,addThreads, freeThreads, hookedThreads,oneThread, killChilds

-- * Additional operators
, (**>), (<**),(<***), (<|)

-- * exceptions

,onException, cutExceptions, continue

-- * Utilities
, StreamData(..)
,genId
)

where


import    Transient.Internals
