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
-- | See http://github.com/agocorona/transient
-----------------------------------------------------------------------------

module Transient.Base(

TransIO(..), TransientIO
,keep, keep', stop
,option, input, exit
,async,waitEvents, spawn, parallel
,react

,setData,getData,getSData,delData

, threads,addThreads, freeThreads, hookedThreads,oneThread, killChilds

, (<**),(<***)

, StreamData(..)
,genId)

where


import    Transient.Internals
