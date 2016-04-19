module Main where

import Transient.Move
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
main = do
     args <- getArgs
     let hostname= read $ head args :: String
         remotehost= read $ args !! 1
         port= 2000
--         numNodes = 2
--         ports = [2000 .. 2000 + numNodes - 1]

         nodes = (createNode "localhost" ports
         node1= head nodes
         node2= nodes !! 1

     runCloud' $ do
         listen (createLocalNode port) <|> return ()
         local $ option "s" "start"
         box <- local newMailBox
         getMailBox box >>= lliftIO . print <|> putMailBox box "hello"


runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()
