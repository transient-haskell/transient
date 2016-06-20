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
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Typeable
import Data.IORef
import Data.List((\\))






-- to be executed with two or more nodes
main = keep $ initNode $ addNode1 <|> test


test= do
        local $ option "exec" "exec"
        nodes <- local getNodes
        when (lenght nodes >1)$ do
           runAt (nodes !! 1) $ print "hello"
           lliftIO $ print "world"




--         box <- local newMailBox
--         getMailBox box >>= lliftIO . print <|> putMailBox box "hello"




