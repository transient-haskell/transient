{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Transient.Move
import Transient.Move.Utils
import Transient.Logged
import Transient.Base
import Transient.Indeterminism
import Transient.EVars
import Network
import Control.Applicative

import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Data.Monoid
import System.IO
import Control.Monad
import Data.Maybe
import Control.Exception hiding (onException)
import Data.Typeable
import Data.IORef
import Data.List((\\))






-- to be executed with two or more nodes
main = keep $ do
        r <- collect 0 $  liftIO $ print "hello"
        liftIO $ print r        