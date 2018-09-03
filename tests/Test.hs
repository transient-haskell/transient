{-# LANGUAGE   CPP, ScopedTypeVariables, DeriveDataTypeable #-}



import Transient.Base
import Transient.EVars
import Transient.Backtrack
import Transient.Indeterminism
import Transient.Internals
import Transient.Logged
import Data.Typeable
import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)

import System.Directory
import System.Random
import Control.Exception
import Control.Concurrent.MVar 
import Control.Concurrent
import Control.Monad.State

main= keep $ do
  
  r <- return ()  `onException'` \(e :: SomeException) -> liftIO $ print e
  return () `catcht` \(e :: SomeException) -> liftIO $ print "CAHTC"
  error "err"
  return()