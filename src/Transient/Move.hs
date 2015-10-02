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
import Transient.Base
import Transient.Logged
import Data.Typeable
import Control.Applicative
import Network


import Control.Monad.IO.Class
import Control.Monad.State
import System.IO
import Control.Exception
import Data.Maybe
import Unsafe.Coerce
import System.Process
import System.Directory
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


import qualified Data.ByteString.Char8 as BS
import System.IO
--import Foreign.Ptr
--import Foreign.Storable
--import Data.ByteString.Internal
--import Foreign.ForeignPtr.Safe

import Control.Concurrent


-- | install in a remote node a haskell package with an executable transient service initialized with `listen`
-- the package, the git repository and the main exectable must have the same name
installService (node@(Node _ port _)) servport package= do
  beamTo node
  liftIO $ do
     let packagename= name package
     exist <- doesDirectoryExist  packagename
     when (not exist) $ do
         runCommand $ "git clone "++ package
         runCommand $ "cd "++ packagename
         runCommand "cabal install"
         createProcess $ shell $ "./dist/build/"++ packagename++"/"++packagename
                                       ++ " " ++ show port
         return()
  where
  name path=
     let x= dropWhile (/= '/') path
     in if x== "" then tail path else name $ tail    x

-- | continue the execution in a new node
-- all the previous actions from `listen` to this statement must have been logged
beamTo :: Node -> TransientIO ()
beamTo node =  do
  Log rec log _ <- getSData <|> return (Log False [][])
  if rec then return () else do
      Connection _ bufSize <- getSData <|> return (Connection Nothing 8192)
      h <-  assign bufSize node
      liftIO $ hPutStrLn h (show $ SMore $ reverse log) >> hFlush h
      release node h
      let log'= WaitRemote: log
      setSData $ Log rec log' log'
      stop

-- | execute in the remote node a process with the same execution state
-- all the previous actions from `listen` to this statement must have been logged
forkTo  :: Node -> TransientIO ()
forkTo node= do
  Log rec log _<- getSData <|> return (Log False [][])
  if rec then return () else do
      Connection _ bufSize <- getSData <|> return (Connection Nothing 8192)
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
            Connection (Just (_, h, sock, blocked)) _ <- getSData  <|> error "callTo: no hander"
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
            Connection _ bufSize <- getSessionData `onNothing` return (Connection Nothing 8192)
            h <- assign bufSize node
            liftIO $ hSetBuffering h LineBuffering
            liftIO $ hPutStrLn h ( show $ SMore $ reverse fulLog) {- >> hFlush h -} !> "CALLTO LOCAL" -- send "++ show  log


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
callTo' :: (Show a, Read a,Typeable a) => Node -> TransIO a -> TransIO a
callTo' node remoteProc= logged $ do
    mynode <- logged getMyNode
    beamTo node
    r <- logged remoteProc
    beamTo mynode
    return r

type Blocked= MVar ()
type BuffSize = Int
data Connection= Connection (Maybe(PortID, Handle, Socket, Blocked)) BuffSize
                  deriving Typeable

setBufSize :: Int -> TransIO ()
setBufSize size= Transient $ do
   Connection c _ <- getSessionData `onNothing` return (Connection Nothing  size)
   setSessionData $ Connection c size
   return $ Just ()


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
listen  (Node _  port _) = do
   addThreads 1
   setSData $ Log False [] []
   Connection _ bufSize <- getSData <|> return (Connection Nothing 8192)
   sock <- liftIO $ withSocketsDo $ listenOn  port
   liftIO $ do NS.setSocketOption sock NS.RecvBuffer bufSize
               NS.setSocketOption sock NS.SendBuffer bufSize
   SMore(h,host,port1) <- parallel $ SMore <$> accept sock
                          `catch` (\(e::SomeException) -> print "socket exception" >> sClose sock >> throw e)

   setSData $ Connection (Just (port, h, sock, unsafePerformIO $ newMVar ())) bufSize -- !> "setdata port=" ++ show port

   liftIO $  hSetBuffering h LineBuffering -- !> "LISTEN in "++ show (h,host,port1)

   mlog <- parallel $ readHandler h

   case  mlog of
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
beamInit :: Node  -> TransIO a -> IO b
beamInit  node program=  keep $ do
    listen  node   <|> return ()
    program




instance Read PortNumber where
  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]


deriving instance Read PortID
deriving instance Typeable PortID



data Pool=  Pool{free :: [Handle], pending :: Int}

data Node= Node{host :: HostName, port :: PortID, connection :: IORef Pool} deriving Typeable

release (Node h p rpool) hand= liftIO $ do
  mhs <- atomicModifyIORef rpool $
            \(Pool hs pend) ->
               if pend==0
                 then (Pool [] 0,Just hs)
                 else (Pool (hand:hs) pend,Nothing)
  case mhs of
    Nothing -> return ()
    Just hs  -> mapM_ hClose hs


assign bufSize (Node h p pool)= liftIO $ do
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
createNode h p= Node h ( PortNumber $ fromInteger p) (unsafePerformIO emptyPool)

instance Eq Node where
    Node h p _ ==Node h' p' _= h==h' && p==p'

instance Show Node where show (Node h p _)= show (h,p)

instance Read Node where
     readsPrec _ s=
          let [((h,p),s')]= readsPrec 0 s
          in [(Node h p empty,s')]
          where
          empty= unsafePerformIO  emptyPool

nodeList :: TVar  [Node]
nodeList = unsafePerformIO $ newTVarIO []

deriving instance Ord PortID

myNode= unsafePerformIO $ newIORef Nothing

setMyNode node= liftIO $ writeIORef  myNode $ Just node
getMyNode= Transient $ liftIO $ readIORef myNode

getNodes :: MonadIO m => m [Node]
getNodes  = liftIO $ atomically $ readTVar  nodeList

addNodes   nodes=  liftIO . atomically $ do
  prevnodes <- readTVar nodeList
  writeTVar nodeList $ nub $ nodes ++ prevnodes


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

-- | Initiates the transient monad, initialize it as a new node (first parameter) and connect it
-- to an existing node (second parameter).
-- The other node will notify about this connection to
-- all the nodes connected to him. this new connected node will receive the list of nodes
-- the local list of nodes then is updated with this list. it can be retrieved with `getNodes`
connect ::  Node ->  Node -> TransientIO ()
connect  (node@(Node h port _))  remotenode=  do
    listen node <|> return ()
    logged $ do
        logged $ do
             setMyNode node
             addNodes [node]
             liftIO $ putStrLn $ "connecting to: "++ show remotenode
        newnode <- logged $ return node -- must pass my node the remote node or else it will use his own
        port <- logged $ return port
        nodes <- callTo remotenode $ do
                   clustered $  addNodes [newnode]
                   r <- getNodes
                   liftIO $ putStrLn $ "Connected to modes: " ++ show r
                   return r

        logged $ addNodes nodes



