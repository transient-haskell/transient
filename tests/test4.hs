import Transient.Move
import Transient.Base
import Transient.Logged
import Transient.Indeterminism
import System.Environment
import Control.Applicative
import Data.Monoid
import Control.Monad.IO.Class
import Control.Concurrent
import Data.IORef
import System.IO.Unsafe

main2= do
    args <- getArgs
    let localPort = read (args !! 0)
        mynode    = createNode "localhost" localPort
        node1  = createNode "localhost" 2000
        node2  = createNode "localhost" 2001
        node3  = createNode "localhost" 2002

    runCloud' $ do
       listen mynode  <|> return ()
       r <- mclustered $ lliftIO $ return "hello"
       lliftIO $ print r

runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()

main= do
     let numNodes = 1
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports

     runCloud' $ do
          local $ addNodes nodes
          runNodes nodes
          r <-   mclustered $ watch <|> sender
          lliftIO $ print r

   where
   counter= unsafePerformIO $ newMVar (0 :: Int)
   watch= local $ do
     n <- liftIO $ modifyMVar counter $ \r -> return (r+1, r+1)
     if n==1 then empty
     else  getMailBox :: TransIO String

   sender= do
     putMailBox "hello "
     putMailBox "world "
     empty




