import Transient.Move
import Transient.Base
import Transient.EVars
import Transient.Logged
import Transient.Indeterminism
import System.Environment
import Control.Applicative
import Data.Monoid
import Control.Monad.IO.Class
import Control.Concurrent
import Data.IORef
import System.IO.Unsafe
import Network(PortID(..))
import Control.Concurrent.STM
--import Network.Socket.Types(PortNumber)

main= do
    args <- getArgs
    let localPort = read (args !! 0)
        mynode = createNode "localhost" localPort
        node1  = createNode "localhost" 2000
        node2  = createNode "localhost" 2001
        node3  = createNode "localhost" 2002

    runCloud' $ do
       box <- local newMailBox
       listen mynode  <|> return ()


       local $ option "fire" "fire"
       box <- onAll $ liftIO $ newTVarIO ""  !> "netTVAR" :: Cloud (TVar String)
       r <- runAt node2 (local $ rec box) <|> runAt node2 (local $ send box)
       lliftIO $ print r

rec box=   waitEvents $ atomically $ do
                           r <- readTVar box
                           if null r then retry !> "retry" else return r

send box=    ( (async $ atomically $ writeTVar box $  "hello") >> empty )

             <|> ((async $ atomically $ writeTVar box $  "world" ) >> empty)



runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()


main2 = do
     let  numNodes = 2
          ports = [2000 .. 2000 + numNodes - 1]
          createLocalNode = createNode "localhost"
          nodes = map createLocalNode ports
          node1= head nodes
          node2= nodes !! 1

     runCloud' $ do
          onAll $ addNodes nodes
--          box <- local $ newMailBox
--          local $ do
--
          runNodes nodes
  --          lliftIO $ print box
--          local $ do
          box <- onAll $ liftIO $ newTVarIO "" :: Cloud (TVar String)
          r <- runAt node1 (local $ rec box) <|> runAt node1 (local $ send box)
          lliftIO $ print r







effects :: MVar [(PortID,String)]
effects= unsafePerformIO $ newMVar []

getEffects ::  Cloud [(PortID,String)]
getEffects = lliftIO $ readMVar effects

printEffects= do
    nods <- getEffects
    lliftIO $ modifyMVar_ effects $ \nods -> do
        print nods
        return []


effect x= do
   Node{nodePort=  n} <- getMyNode
   return () !> (n,x)
--   lliftIO $ modifyMVar_ effects $ \xs -> return $ (n,x):xs
