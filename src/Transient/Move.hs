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
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable , ExistentialQuantification
    ,ScopedTypeVariables, StandaloneDeriving #-}
module Transient.Move where
import Transient.Base
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

data IDynamic= IDyns String | forall a.(Read a, Show a,Typeable a) => IDynamic a

instance Show IDynamic where
  show (IDynamic x)= show $ show x
  show (IDyns s)= show s

instance Read IDynamic where
  readsPrec n str= map (\(x,s) -> (IDyns x,s)) $ readsPrec n str


fromIDyn :: (Read a, Show a, Typeable a) => IDynamic -> a
fromIDyn (IDynamic x)= unsafeCoerce x

fromIDyn (IDyns s)= read s !> "read " ++ s

toIDyn x= IDynamic x



type Recover= Bool
data LogElem= Exec | Step IDynamic deriving (Read,Show)
type CurrentPointer= [LogElem]
type LogEntries= [LogElem]

data Log= Log Recover  CurrentPointer LogEntries deriving Typeable

step :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
step mx= Transient $ do

    Log recover  rs full <- getSessionData `onNothing` return ( Log False  [][])
--    liftIO $ putStrLn $ "step "++ showhead rs
    case (recover,rs) of
      (True, Step x: rs') -> do
            setSData $ Log recover  rs' full
            return. Just $ fromIDyn x

      (True,Exec:rs') -> do
            setSData $ Log recover rs' full
            runTrans mx

      _ -> do

        let add= Exec:  rs
        setSData $ Log False add add


        r <- runTrans mx
        let add= Step (toIDyn r): full
        setSData $ Log False add add
        return  r



-- | install in a remote node a haskell package with an executable transient service initialized with `listen`
-- the package, the git repository and the main exectable must have the same name
installService node port servport package= do
  beamTo node port
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


beamTo :: HostName -> PortID -> TransientIO ()
beamTo node port= do
  Log rec log _<- getSData <|> return (Log False [][])
  if rec then return () else do
      h <- liftIO $ connectTo node port
      liftIO $ hSetBuffering h LineBuffering
      liftIO $ hPutStrLn h (show $ reverse log)  >> hFlush h
      liftIO $ hClose h
      delSData h
      stop

forkTo  :: HostName -> PortID -> TransientIO ()
forkTo node port= do
  Log rec log _<- getSData <|> return (Log False [][])
  if rec then return () else do
      h <- liftIO $ connectTo node port
      liftIO $ hSetBuffering h LineBuffering
      liftIO $ hPutStrLn h (show $ reverse log)  >> hFlush h
      liftIO $ hClose h
      delSData h

callTo :: (Show a, Read a,Typeable a) => HostName -> PortID -> TransientIO a -> TransientIO a
callTo node port remoteProc= step $ Transient $ do
      liftIO $ print "CALLTO"
      Log rec log fulLog<- getSessionData `onNothing` return (Log False [][])
      if rec !> "logdata="++ show (reverse fulLog)
         then do -- remote side
           liftIO $ putStrLn "remote"
--           setSData $ Log False log fulLog
           r <- runTrans remoteProc !> "remoteProc"
           (h,sock) <- getSessionData `onNothing` error "callto: no hander"  !> "executed remoteProc"
           liftIO $ hPutStrLn h (show r)  `catch` (\(e::SomeException) -> sClose sock) !> "sent response"
           return empty

         else runTrans $ async $ do -- local side

           h <- connectTo node port
           hPutStrLn h (show $ reverse fulLog) >> hFlush h !> "local" !> "send "++ (show $ reverse fulLog)
           liftIO $ hSetBuffering h LineBuffering
           s <- hGetLine h
           hClose h

           return $  read s   !> "read: " ++ s



listen  port = do
--       liftIO $ print "listen"
       sock <- liftIO $ listenOn  port

       (h,_,_) <- parallel $ Right <$> accept sock
       liftIO $ hSetBuffering h LineBuffering
--       liftIO $ print "received something"
       slog <- liftIO $ hGetLine  h
----       liftIO $ putStrLn $ "Listen rec=" ++ show (length slog) ++ slog
       setSData (h,sock)
       let log= read slog  !> "read1 " ++ slog
       liftIO $ print $ head log
       liftIO $ print "after read"
       setSData $ Log True log  (reverse log)

beamInit :: PortID -> Transient StateIO a -> IO b
beamInit port program=  keep $ do
    listen port  <|> return ()
    (program >> empty)  <|> close
    where
    close= do
       (h,sock) <- getSData
       liftIO $ hClose h  `catch` (\(e::SomeException) -> sClose sock)

-- longCallTo: for very long processes that return the result to the calling method

--instance Read PortID where
--
--Service String	
--PortNumber PortNumber	
--UnixSocket String


instance Read PortNumber where
  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]




deriving instance Read PortID
deriving instance Typeable PortID

type Node= (HostName,PortID)

nodeList = unsafePerformIO $ newTVarIO ([] :: [Node])

myNode= unsafePerformIO $ newTVarIO ("", PortNumber $ fromIntegral 0)

getNodes :: TransientIO [Node]
getNodes= liftIO $ atomically $ readTVar nodeList

connect ::   HostName ->  PortID -> Maybe HostName -> Maybe PortID -> TransientIO ()
connect   host port Nothing Nothing= step $ Transient $ do
    liftIO $ atomically $ do
        writeTVar nodeList [(host, port)] !> "connect nothing"
        writeTVar myNode (host,port)

    liftIO $ print "connect NOTHING"
    runTrans $ async $ atomically $ do
       ns <- readTVar nodeList
       case ns of
         [_] -> STM.retry
         _   -> return  ()


connect   host  port  (Just remotehost) (Just remoteport)= step $ do
--   nodes <-  getNodes !> "connect"
--   host <- step $ do
--      ifs <- liftIO $ getNetworkInterfaces
--      liftIO $ mapM_ (\(i,n) ->putStrLn $ show i ++ "\t"++  show (ipv4 n) ++ "\t"++name n)$ zip [0..] ifs
--      ind <-  input (< 5)
--      return $ show . ipv4 $ ifs !! ind

    step $ liftIO $ putStrLn $ "connecting to: "++ show (remotehost,remoteport)
    step $ liftIO $ putStrLn $ "to add node " ++ show (host,port)
    nodes <- callTo remotehost remoteport $ do
           nodes <- liftIO $ atomically $ readTVar nodeList
           foldl (<>) mempty $ map (\(h,p) -> addNode h p host port) $  nodes

--           callTo remotehost remoteport $ liftIO $ atomically $ writeTVar  nodeList ((host,port): nodes)
           step $ liftIO $ putStrLn "CONNECT BEFORE RETURN"
           return $ (remotehost,remoteport):nodes

    step $ liftIO $ atomically $ do
           writeTVar nodeList nodes
           writeTVar myNode (host,port)
           return  ()
    where
    addNode h p h' p'= callTo h p . liftIO $ do
       liftIO $ putStrLn $ "adding node" ++ show (h',p')++ "to"++ show (h,p)
       liftIO $ atomically $ readTVar nodeList >>= \nodes -> writeTVar  nodeList ((h',  p'): nodes)
       return ()








