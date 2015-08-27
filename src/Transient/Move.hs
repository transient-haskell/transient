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
    ,ScopedTypeVariables, StandaloneDeriving #-}
module Transient.Move where
import Transient.Base
import Transient.Logged
import Data.Typeable
import Control.Applicative
import Network
import Network.HTTP
import Control.Monad.IO.Class
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
import Data.Monoid
import qualified Data.Map as M
import Data.List (nub,(\\),find)
import Data.IORef



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
      h <-  assign node
      liftIO $ hSetBuffering h LineBuffering
      liftIO $ hPutStrLn h (show $ reverse log) >> hFlush h
      release node h
      let log'= WaitRemote: log
      setSessionData $ Log rec log' log'
      stop

-- | execute in the remote node a process with the same execution state
-- all the previous actions from `listen` to this statement must have been logged
forkTo  :: Node -> TransientIO ()
forkTo node= do
  Log rec log _<- getSData <|> return (Log False [][])
  if rec then return () else do
      h <-assign node
      liftIO $ hSetBuffering h LineBuffering
      liftIO $ hPutStrLn h (show $ reverse log)  >> hFlush h
      release node h

-- | executes an action in another node.
-- all the previous actions from `listen` to this statement must have been logged
callTo :: (Show a, Read a,Typeable a) => Node -> TransIO a -> TransIO a
callTo node remoteProc= logged $ Transient $ do
--      liftIO $ print "callto"
      Log rec log fulLog <- getSessionData `onNothing` return (Log False [][])
      if rec
         then
          runTrans $ do

               Connection port  h sock <- getSData <|> error "callto: no hander"
               r <- remoteProc !> "executing remoteProc" !> "CALLTO REMOTE" -- LOg="++ show fulLog
               liftIO $ hPutStrLn h (show r)  `catch` (\(e::SomeException) -> sClose sock)
                 -- !> "sent response, HANDLE="++ show h
               setSData WasRemote
               stop


         else do

            h <- assign node
            liftIO $ hPutStrLn h (show $ reverse fulLog) >> hFlush h !> "CALLTO LOCAL" -- send "++ show  log
            let log'= WaitRemote:tail log
            setSessionData $ Log rec log' log'
            runTrans $ waitEvents $ do -- local side
                   liftIO $ hSetBuffering h LineBuffering
                   s <- hGetLine h
                   release node h

                   let r = read s

                   return r   !> "read: " ++ s ++" response type= "++show( typeOf r)

-- | synonymous of `callTo`
-- all the previous actions from `listen` to this statement must have been logged
runAt :: (Show a, Read a,Typeable a) => Node -> TransIO a -> TransIO a
runAt= callTo

-- | A connectionless version of callTo for long running remote calls
callTo' :: (Show a, Read a,Typeable a) => Node -> TransIO a -> TransIO a
callTo' node remoteProc= logged $ do
    mynode <- logged getMyNode
    beamTo node
    r <- logged remoteProc
    beamTo mynode
    return r

data Connection= Connection PortID Handle Socket deriving Typeable

-- | Wait for messages and replay the rest of the monadic sequence with the log received.
listen :: PortID ->  TransIO ()
listen  port = do
   setSData $ Log False [] []
   sock <- liftIO $ withSocketsDo $ listenOn  port

   (h,host,port1) <- parallel $ Right <$> accept sock
                          `catch` (\(e::SomeException) -> print "socket exception" >> sClose sock >> throw e)

   setSData $ Connection port h sock  -- !> "setdata port=" ++ show port

   liftIO $  hSetBuffering h LineBuffering  -- !> "LISTEN in "++ show (h,host,port1)

   log <- parallel $ process h sock
   if null log
     then stop
     else setSData $ Log True log (reverse log)

   where
   process h sock= loop'
     where
     loop'= do
       mslog <- (Just <$> hGetLine  h)
                  `catch` (\(e::SomeException) -> hClose h >>  return Nothing)

       case mslog of
         Nothing -> return $ Left []
         Just slog ->do
            let log= read slog   -- !> "read1 " ++ slog

            return $ Right log



-- | init a Transient process in a interactive as well as in a replay mode.
-- It is intended for twin processes that interact among them in different nodes.
beamInit :: PortID -> TransIO a -> IO b
beamInit port program=  keep $ do
    listen port   <|> return ()
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


assign (Node h p pool)= liftIO $ do
    mh <- atomicModifyIORef pool $
            \(Pool hs p) ->  if null hs then (Pool hs p, Nothing)
                                        else (Pool (tail hs) p, Just(head hs)) !> "REUSED"
    case mh of
      Just handle -> liftIO (putStrLn "REUSED!") >> return handle
      Nothing -> liftIO $ connectTo h p     !>  "REOPEN"





-- * Level 2: connections node lists and operations with the node list


{-# NOINLINE emptyPool #-}
emptyPool :: MonadIO m => m (IORef Pool)
emptyPool= liftIO $ newIORef $ Pool [] 0

createNode h p= Node h p (unsafePerformIO emptyPool)

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


-- | execute a Transient action in each of the nodes connected. The results are aggregated with `mappend`
clustered :: (Typeable a, Show a, Read a) => Monoid a => TransIO a -> TransIO a
clustered proc= logged $ do
     nodes <- step getNodes
     logged $ foldr (<>) mempty $ map (\node -> callTo node proc) nodes !> "fold"

-- | a connectionless version of clustered for long running remote computations. Not tested
clustered' proc= logged $ do
     nodes <-  getNodes
     logged $ mapM (\node -> callTo' node proc) $ nodes

-- | Connect to a new node to another. The other node will notify about this connection to
-- all the nodes connected to him. the new connected node will receive the list of connected nodes
-- the nodes will be updated with this list. it can be retrieved with `getNodes`
connect ::   Node ->  Node -> TransientIO ()
connect (node@(Node h port _))  remotenode=  do
    listen port <|> return ()
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



