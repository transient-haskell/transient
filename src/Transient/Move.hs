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
import qualified Data.Map as M
import Data.List (nub)

data IDynamic= IDyns String | forall a.(Read a, Show a,Typeable a) => IDynamic a

instance Show IDynamic where
  show (IDynamic x)= show $ show x
  show (IDyns s)= show s

instance Read IDynamic where
  readsPrec n str= map (\(x,s) -> (IDyns x,s)) $ readsPrec n str


fromIDyn :: (Read a, Show a, Typeable a) => IDynamic -> a
fromIDyn (IDynamic x)= unsafeCoerce x

fromIDyn (IDyns s)=r where r= read s !> "read " ++ s ++ "to type "++ show (typeOf r)

toIDyn x= IDynamic x



type Recover= Bool
data LogElem= Exec | Step IDynamic deriving (Read,Show)
type CurrentPointer= [LogElem]
type LogEntries= [LogElem]

data Log= Log Recover  CurrentPointer LogEntries deriving Typeable

step :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
step mx=  do
    Log recover rs full <- getSData <|> return ( Log False  [][])
--    liftIO $ putStrLn $ "step "++ show rs ++ "recover=" ++ show recover
    case (recover,rs) of
      (True, Step x: rs') -> do
--            liftIO $ print "READ FROM LOg"
            setSData $ Log recover rs' full
            return $ fromIDyn x !>  "read in step:" ++ show x 

      (True,Exec:rs') -> do
--            liftIO $ print "EXeC1"
            setSData $ Log recover rs' full
            mx

      _ -> do
--            liftIO $ print "EXeC2"
            let add= Exec:  full
            setSData $ Log False add add
            r <-  mx
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
beamTo host port= do
  Log rec log _<- getSData <|> return (Log False [][])
  if rec then return () else do
      h <- liftIO $ connectTo host port
      liftIO $ hSetBuffering h LineBuffering
      liftIO $ hPutStrLn h (show $ reverse log) >> hFlush h
      liftIO $ hClose h
      delSData h
      stop

forkTo  :: HostName -> PortID -> TransientIO ()
forkTo host port= do
  Log rec log _<- getSData <|> return (Log False [][])
  if rec then return () else do
      h <- liftIO $ connectTo host port
      liftIO $ hSetBuffering h LineBuffering
      liftIO $ hPutStrLn h (show $ reverse log)  >> hFlush h
      liftIO $ hClose h
      delSData h

callTo :: (Show a, Read a,Typeable a) => HostName -> PortID -> TransientIO a -> TransientIO a
callTo host port remoteProc=  step $ Transient $ do
      Log rec _ fulLog <- getSessionData `onNothing` return (Log False [][])
      if rec 
         then do -- remote side
--           liftIO $ do putStrLn ( "CALLTO remote logdata="++ show (reverse fulLog)) ; getLine
           
----           setSData $ Log False log fulLog
           r <- runTrans $  remoteProc  !> "remoteProc"
           Connection port  h sock <- getSessionData `onNothing` error "callto: no hander"  !> "executed remoteProc"
           liftIO $ hPutStrLn h (show r)  `catch` (\(e::SomeException) -> sClose sock) !> "sent response"
           return Nothing

         else runTrans $  do
              mr <-async $ do -- local side
                   h <- connectTo host port
                   hPutStrLn h (show $ reverse fulLog) >> hFlush h
--                   liftIO $  do putStrLn ( "CALLTO local send "++ (show $ reverse fulLog)) 
                   liftIO $ hSetBuffering h LineBuffering
                   s <- hGetLine h
                   hClose h

                   let r = read s
                    
                   return r   !> "read: " ++ s ++" response type= "++show( typeOf r) 
              case mr of
                Nothing -> empty
                Just r -> return r

data Connection= Connection PortID Handle Socket deriving Typeable

-- | Wait for messages and replay the rest of the monadic sequence with the log received.
listen :: PortID ->  TransIO ()
listen  port = do
--       liftIO $ print "LISTEN"
       sock <- liftIO $ listenOn  port

       (h,_,_) <- parallel $ Right <$> accept sock
       liftIO $ hSetBuffering h LineBuffering
--       liftIO $ print "received something"
       slog <- liftIO $ hGetLine  h
--       liftIO $ putStrLn $ "Listen rec=" ++ show (length slog) ++ slog
       setSData $ Connection port h sock !> "setdata port=" ++ show port

       let log= read slog  !> "read1 " ++ slog
       setSData $ Log True log (reverse log)
       


-- | init a Transient process in a interactive as well as in a replay mode.
-- It is intended for twin processes that interact among them in different nodes.
beamInit :: PortID -> TransIO a -> IO b
beamInit port program=  keep $ do
    listen port  <|> return ()
    (program >> empty)  <|> close
    where
    close= do
       (_ :: PortID,h,sock) <- getSData
       liftIO $ hClose h  `catch` (\(e::SomeException) -> sClose sock)




instance Read PortNumber where
  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]




deriving instance Read PortID
deriving instance Typeable PortID

type Node= (HostName,PortID)

nodeList :: TVar (M.Map PortID  [Node])
nodeList = unsafePerformIO $ newTVarIO $ M.empty

myNode :: TVar (M.Map PortID HostName)
myNode= unsafePerformIO $ newTVarIO  M.empty

getAllNodes :: TransientIO [Node]
getAllNodes= liftIO $ atomically $ readTVar nodeList >>= return . nub . concat . M.elems
--
--getMyNode= liftIO $atomically $ readTVar myNode


deriving instance Ord PortID

getMyNode port= Transient $ do
  myNodes <- liftIO $ atomically $ readTVar myNode
  let mx = M.lookup port myNodes
  case mx of
    Nothing -> return Nothing
    Just host -> return $ Just (host,port)

setMyNode  (host,port)= Transient . liftIO . atomically $ do
  myNodes <-  readTVar myNode
  writeTVar myNode  $ M.insert port  host myNodes
  return $ Just ()

getNodes port = Transient $ do
  nodes <- liftIO $ atomically $ readTVar  nodeList
  return $ M.lookup port nodes



addNodes port  nodes= Transient . liftIO . atomically $ do
  mnodes <- readTVar nodeList
  let Just prevnodes= M.lookup port mnodes  <|> Just []
  writeTVar nodeList $ M.insert port (nub $ nodes ++ prevnodes)  mnodes
  return $ Just ()  !> "added node "++ show nodes ++" to "++ show prevnodes
                                    ++ " for port " ++ show port


instance Read Socket where readsPrec = error $ "Socket Read instance undefined"
instance Read Handle where readsPrec= error $ "Handle Read instance undefined"
clustered :: (Typeable a, Show a, Read a) => Monoid a => TransientIO a -> TransientIO a
clustered proc= step $ do
     Connection port _ _ <-  getSData <|> error "clustered: No port set" !> "clustered"

     nodes <- step $ getNodes port  !> "getnode"
     step $ foldl (<>) mempty $ map (\(h,p) -> callTo h p proc) nodes !> "fold"

--getInterfaces :: TransIO Logged HostName
--getInterfaces= do
--   host <- step $ do
--      ifs <- liftIO $ getNetworkInterfaces
--      liftIO $ mapM_ (\(i,n) ->putStrLn $ show i ++ "\t"++  show (ipv4 n) ++ "\t"++name n)$ zip [0..] ifs
--      liftIO $ putStrLn "Select one: "
--      ind <-  input ( < length ifs)
--      return $ show . ipv4 $ ifs !! ind

connect ::   HostName ->  PortID ->  HostName ->  PortID -> TransientIO ()
connect host  port   remotehost remoteport=do
            listen port <|> return ()
            step $ setMyNode  (host,port)
            step $ addNodes port [(host,port)]
            step $ liftIO $ putStrLn $ "connecting to: "++ show (remotehost,remoteport)

            host <- step $ return host
            port <- step $ return port
            nodes <- callTo remotehost remoteport   $ do
                       Connection portd _ _ <-  getSData  <|> error "NO PORT"
                       step $ addNodes portd  [(host, port)]
                       myNode <- step $ getMyNode portd
                       return [myNode]
                    
            step $ addNodes port nodes
            step $ liftIO $ putStrLn $ "Connected to modes: " ++ show nodes

   







