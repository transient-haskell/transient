-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move
-- Copyright   :
-- License     :  GPL-3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | see <https://www.fpcomplete.com/user/agocorona/moving-haskell-processes-between-nodes-transient-effects-iv>
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable , ExistentialQuantification
    ,ScopedTypeVariables, StandaloneDeriving, RecordWildCards #-}
module Transient.Move where
import Transient.Base hiding (onNothing)
import Transient.Logged
import Transient.EVars
import Data.Typeable
import Control.Applicative
import Network


import Control.Monad.IO.Class
import Control.Monad.State
import System.IO
import Control.Exception
import Data.Maybe
import Unsafe.Coerce

--import System.Directory
import Control.Monad
import Network.Info
import System.IO.Unsafe
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar

import Data.Monoid
import qualified Data.Map as M
import Data.List (nub,(\\),find)
import Data.IORef

import qualified Network.Socket as NS
import qualified Network.BSD as BSD


import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO

import Control.Concurrent

import Data.TCache
import Data.TCache.DefaultPersistence


-- | continue the execution in a new node
-- all the previous actions from `listen` to this statement must have been logged
beamTo :: Node -> TransientIO ()
beamTo node =  do
  Log rec log _ <- getSData <|> return (Log False [][])
  if rec then return () else do
      Connection{bufferSize=bufSize}
        <- getSData
              <|> return (Connection beamToErr Nothing 8192 beamToErr)
      h <-  assign bufSize node
      liftIO $ hPutStrLn h (show $ SMore $ reverse log) >> hFlush h
      release node h
      let log'= WaitRemote: log
      setSData $ Log rec log' log'
      stop
  where
  beamToErr= error "beamTo does not set some necessary params use setMyNode and listen"
-- | execute in the remote node a process with the same execution state
-- all the previous actions from `listen` to this statement must have been logged
forkTo  :: Node -> TransientIO ()
forkTo node= do
  Log rec log _<- getSData <|> return (Log False [][])
  if rec then return () else do
      Connection {bufferSize=bufSize}  <- getSData <|> return (Connection undefined Nothing 8192 undefined)
      h <-assign bufSize node
      liftIO $ hPutStrLn h (show $ SMore $ reverse log)  >> hFlush h
      release node h

-- | executes an action in another node.
-- All the previous actions from `listen` to this statement must have been logged
callTo :: Loggable a => Node -> TransIO a -> TransIO a
callTo n p = streamFrom n (SMore <$> p) >>= \(SMore x) -> return x


-- | synonymous of `callTo`
-- all the previous actions from `listen` to this statement must have been logged
runAt :: Loggable a => Node -> TransIO a -> TransIO a
runAt= callTo

-- | `callTo` can stream data but can not inform the receiving process about the finalization. This call
-- does it.
--
-- All the previous actions from `listen` to this statement must have been logged
streamFrom :: Loggable a => Node -> TransIO (StreamData a) -> TransIO (StreamData a)
streamFrom node remoteProc= logged $ Transient $ do
      Log rec log fulLog <- getSessionData `onNothing` return (Log False [][])
      if rec
         then
          runTrans $ do
            rnum <- liftIO $ newMVar (0 :: Int)
            Connection _(Just (ConnectionData _ h sock blocked )) _ _ <- getSData  <|> error "callTo: no hander"
            r <- remoteProc                       !> "executing remoteProc" !> "CALLTO REMOTE" -- LOg="++ show fulLog
            n <- liftIO $ do
--                modifyMVar_ rnum $ \n -> return (n+1)
                withMVar blocked $ const $ hPutStrLn   h (show r)  `catch` (\(e::SomeException) -> sClose sock)
                 -- !> "sent response, HANDLE="++ show h
--                modifyMVar rnum $ \n -> return (n-1,n)

 --           adjustSenderThreads n

            setSData WasRemote
            stop

         else do
            Connection _ _ bufSize _<- getSessionData `onNothing` return (Connection undefined Nothing 8192 undefined)
            h <- assign bufSize node
            liftIO $ hSetBuffering h LineBuffering
            liftIO $ hPutStrLn h ( show $ SLast $ reverse fulLog) {- >> hFlush h -} !> "CALLTO LOCAL" -- send "++ show  log


            let log'= WaitRemote:tail log
            setSessionData $ Log rec log' log'
            runTrans $ do
              r<- parallel $ do -- local side
                   r <- readHandler h

                   case r of
                        SDone -> release node h >> return SDone
                        other -> return other

--              adjustRecThreads h
              case r of
                SDone -> empty
                other  -> return other

--      where
--      adjustRecThreads h= do
--          b <- liftIO $ hWaitForInput  h 1
--          addThreads' $ if b then 1 else 0
--          liftIO $ putStrLn $ "REC "++ show (case b of True -> "INC" ; _ -> "DEC")
--
--      adjustSenderThreads n
--         | n > 2 = addThreads' (-1)  >> liftIO (putStrLn ("SEND DEC"))
--         | n==0 = addThreads' 1  >> liftIO (putStrLn ("SEND INC"))
--         | otherwise= return () >> liftIO(myThreadId >>= \th -> (putStrLn ("SEND "++ show th)))


-- | A connectionless version of callTo for long running remote calls
-- myNode should be set with `setMyNode`
callTo' :: (Show a, Read a,Typeable a) => Node -> TransIO a -> TransIO a
callTo' node remoteProc= logged $ do
    mynode <- logged getMyNode
    beamTo node
    r <- logged remoteProc
    beamTo mynode
    return r

type Blocked= MVar ()
type BuffSize = Int
data ConnectionData= ConnectionData{port :: PortID
                                   ,handle :: Handle
                                   ,socket ::Socket
                                   ,blocked :: Blocked}




data Connection= Connection{myNode :: DBRef MyNode
                           ,connData :: (Maybe(ConnectionData))
                           ,bufferSize ::BuffSize
                           ,comEvent :: EVar(Node,Service)}
                  deriving Typeable

setBufSize :: Int -> TransIO ()
setBufSize size= Transient $ do
   Connection n c _ ev <- getSessionData `onNothing`
              return (Connection (error "myNode not set: use setMyNode") Nothing  size (error "accessing network events out of listen"))
   setSessionData $ Connection n c size ev
   return $ Just ()
getBuffSize=
  (do Connection _ _ bufSize _ <- getSData ; return bufSize) <|> return  8192
readHandler h= do
    line <- hGetLine h -- {-  readIORef leftover <> -} unsafeInterleaveIO (hGetLine h)
--    liftIO $ print (line, show h)
    let [(v,left)]= readsPrec 0 line
--    length v `seq` writeIORef leftover left
    return  v

  `catch` (\(e::SomeException) -> do
      hClose h
      liftIO $ do
         putStr "readHandler: "
         print e
         return SDone)
   where
--   hGetLine' h= do



connectTo' bufSize hostname (PortNumber port) = do
    proto <- BSD.getProtocolNumber "tcp"
    bracketOnError
        (NS.socket NS.AF_INET NS.Stream proto)
        (sClose)  -- only done if there's an error
        (\sock -> do
          NS.setSocketOption sock NS.RecvBuffer bufSize
          NS.setSocketOption sock NS.SendBuffer bufSize
          he <- BSD.getHostByName hostname
          NS.connect sock (NS.SockAddrInet port (BSD.hostAddress he))

          NS.socketToHandle sock ReadWriteMode
        )

-- | Wait for messages and replay the rest of the monadic sequence with the log received.
listen ::  Node ->  TransIO ()
listen  (Node _  port _ _) = do
   addThreads 1

   setSData $ Log False [] []

   Connection node _ bufSize events
        <- getSData
              <|>  do events <- newEVar
                      return (Connection (error "nyNode not set: use setMyNode") Nothing 8192 events)
   sock <- liftIO $ withSocketsDo $ listenOn  port
   liftIO $ do NS.setSocketOption sock NS.RecvBuffer bufSize
               NS.setSocketOption sock NS.SendBuffer bufSize
   SMore(h,host,port1) <- parallel $ SMore <$> accept sock
                          `catch` (\(e::SomeException) -> print "socket exception" >> sClose sock >> throw e)

   setSData $ Connection node (Just (ConnectionData port h sock (unsafePerformIO $ newMVar ()))) bufSize events -- !> "setdata port=" ++ show port

   liftIO $  hSetBuffering h LineBuffering -- !> "LISTEN in "++ show (h,host,port1)

   mlog <- parallel $ readHandler h

   case  mlog  of
         SError e -> do
             liftIO $ do
                hClose h
                putStr "listen: "
                print e
             stop

         SDone -> liftIO (hClose h) >> stop
         SMore log -> setSData $ Log True log (reverse log)
         SLast log -> setSData $ Log True log (reverse log)



-- | init a Transient process in a interactive as well as in a replay mode.
-- It is intended for twin processes that interact among them in different nodes.
beamInit :: Node  -> TransIO a -> IO a
beamInit  node program=  keep $ do
    listen  node   <|> return ()
    program

instance Read PortNumber where
  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]


deriving instance Read PortID
deriving instance Typeable PortID



data Pool=  Pool{free :: [Handle], pending :: Int}
type Package= String
type Program= String
type Service= (Package, Program, Int)

data Node= Node{ nodeHost   :: HostName
               , nodePort   :: PortID
               , connection :: IORef Pool
               , services   :: [Service]}
               deriving Typeable


release (Node h p rpool _) hand= liftIO $ do
  mhs <- atomicModifyIORef rpool $
            \(Pool hs pend) ->
               if pend==0
                 then (Pool [] 0,Just hs)
                 else (Pool (hand:hs) pend,Nothing)
  case mhs of
    Nothing -> return ()
    Just hs  -> mapM_ hClose hs


assign bufSize (Node h p  pool _)= liftIO $ do
    mh <- atomicModifyIORef pool $
            \(Pool hs p) ->  if null hs then (Pool hs p, Nothing)
                                        else (Pool (tail hs) p, Just(head hs)) !> "REUSED"
    case mh of
      Just handle -> liftIO (putStrLn "REUSED!") >> return handle
      Nothing -> liftIO $ do
                  h <- connectTo' bufSize h p     !>  "REOPEN"
                  hSetBuffering h LineBuffering
                  return h




-- * Level 2: connections node lists and operations with the node list


{-# NOINLINE emptyPool #-}
emptyPool :: MonadIO m => m (IORef Pool)
emptyPool= liftIO $ newIORef $ Pool [] 0

createNode :: HostName -> Integer -> Node
createNode h p= Node h ( PortNumber $ fromInteger p) (unsafePerformIO emptyPool) []

instance Eq Node where
    Node h p _ _ ==Node h' p' _ _= h==h' && p==p'

instance Show Node where show (Node h p _ servs)= show (h,p,servs)

instance Read Node where
     readsPrec _ s=
          let [((h,p,ss),s')]= readsPrec 0 s
          in [(Node h p empty ss,s')]
          where
          empty= unsafePerformIO  emptyPool

newtype MyNode= MyNode Node deriving(Read,Show,Typeable)
instance Indexable MyNode where key (MyNode Node{nodePort=port}) =  "MyNode "++ show port

instance Serializable MyNode where
    serialize= BS.pack . show
    deserialize= read . BS.unpack

nodeList :: TVar  [Node]
nodeList = unsafePerformIO $ newTVarIO []

deriving instance Ord PortID

--myNode :: Int -> DBRef  MyNode
--myNode= getDBRef $ key $ MyNode undefined



getMyNode :: TransIO Node
getMyNode = do
    Connection{myNode=rnode} <- getSData <|> error "getMyNode: Node not set. Use setMynode"
    MyNode node <- liftIO $ atomically $ readDBRef rnode `onNothing` error "getMyNode: Node not set. Use setMynode"
    return node

setMyNode :: Node -> TransIO ()
setMyNode node= do
        events <- newEVar
        rnode <- liftIO $ atomically $ newDBRef $ MyNode node
        let conn= Connection rnode Nothing 8192 events
        setSData conn
        return ()

getNodes :: MonadIO m => m [Node]
getNodes  = liftIO $ atomically $ readTVar  nodeList

addNodes :: MonadIO m => [Node] -> m ()
addNodes   nodes=  liftIO . atomically $ do
  prevnodes <- readTVar nodeList
  writeTVar nodeList $ nub $ nodes ++ prevnodes

shuffleNodes :: MonadIO m => m [Node]
shuffleNodes=  liftIO . atomically $ do
  nodes <- readTVar nodeList
  let nodes'= tail nodes ++ [head nodes]
  writeTVar nodeList nodes'
  return nodes'

--getInterfaces :: TransIO TransIO HostName
--getInterfaces= do
--   host <- logged $ do
--      ifs <- liftIO $ getNetworkInterfaces
--      liftIO $ mapM_ (\(i,n) ->putStrLn $ show i ++ "\t"++  show (ipv4 n) ++ "\t"++name n)$ zip [0..] ifs
--      liftIO $ putStrLn "Select one: "
--      ind <-  input ( < length ifs)
--      return $ show . ipv4 $ ifs !! ind


-- | execute a Transient action in each of the nodes connected.
--
-- The response of each node is returned and processed by the rest of the procedure.
-- By default, the response is processed in a new thread. To restrict the number of threads
-- use the thread control primitives.
--
-- this snippet receive a message from each of the simulated nodes:
-- > main = keep $ do
-- >    let nodes= map createLocalNode [2000..2005]
-- >    addNodes nodes
-- >    (foldl (<|>) empty $ map listen nodes) <|> return ()
-- >
-- >    r <- clustered $ do
-- >               Connection (Just(PortNumber port, _, _, _)) _ <- getSData
-- >               return $ "hi from " ++ show port++ "\n"
-- >    liftIO $ putStrLn r
-- >    where
-- >    createLocalNode n= createNode "localhost" (PortNumber n)
clustered :: Loggable a  => TransIO a -> TransIO a
clustered proc= logged $ do
     nodes <- step getNodes
     logged $ foldr (<|>) empty $ map (\node -> callTo node proc) nodes !> "fold"

-- | a connectionless version of clustered for long running remote computations. Not tested
clustered' proc= logged $ do
     nodes <-  getNodes
     logged $ mapM (\node -> callTo' node proc) $ nodes

-- A variant of clustered that wait for all the responses and `mappend` them
mclustered :: (Monoid a, Loggable a)  => TransIO a -> TransIO a
mclustered proc= logged $ do
     nodes <- step getNodes
     logged $ foldr (<>) mempty $ map (\node -> callTo node proc) nodes !> "fold"

-- | Initiates the transient monad, initialize it as a new node (first parameter) and connect it
-- to an existing node (second parameter).
-- The other node will notify about this connection to
-- all the nodes connected to him. this new connected node will receive the list of nodes
-- the local list of nodes then is updated with this list. it can be retrieved with `getNodes`
connect ::  Node ->  Node -> TransientIO ()
connect  node  remotenode=  do
    listen node <|> return ()
    logged $ do
        logged $ do
             setMyNode node
             addNodes [node]
             liftIO $ putStrLn $ "connecting to: "++ show remotenode
        newnode <- logged $ return node -- must pass my node the remote node or else it will use his own
--        port <- logged $ return port
        nodes <- callTo remotenode $ do
                   mclustered $  addNodes [newnode]
                   getNodes

        liftIO $ putStrLn $ "Connected to modes: " ++ show nodes
        logged $ addNodes nodes




