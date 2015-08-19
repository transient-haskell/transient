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
import Data.List (nub)



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
  Log rec log _ <- getSData <|> return (Log False [][])
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


callTo :: (Show a, Read a,Typeable a) => HostName -> PortID -> TransIO a -> TransIO a
callTo host port remoteProc= logged $ Transient $ do
--      liftIO $ print "callto"
      Log rec log fulLog <- getSessionData `onNothing` return (Log False [][])
      if rec
         then
          runTrans $ do

               Connection port  h sock <- getSData <|> error "callto: no hander"
               r <- remoteProc !> "executing remoteProc" !> "CALLTO REMOTE" -- LOg="++ show fulLog
               liftIO $ hPutStrLn h (show r) --  `catch` (\(e::SomeException) -> sClose sock)
                -- !> "sent response, HANDLE="++ show h
               setSData WasRemote
               stop


         else do

            h <- liftIO $ connectTo host port
            liftIO $ hPutStrLn h (show $ reverse fulLog) >> hFlush h !> "CALLTO LOCAL" -- send "++ show  log
            let log'= WaitRemote:tail log
            setSessionData $ Log rec log' log'
            runTrans $ waitEvents $ do -- local side
                   liftIO $ hSetBuffering h LineBuffering
                   s <- hGetLine h
--                   hClose h

                   let r = read s

                   return r   !> "read: " ++ s ++" response type= "++show( typeOf r)




data Connection= Connection PortID Handle Socket deriving Typeable

-- | Wait for messages and replay the rest of the monadic sequence with the log received.
listen :: PortID ->  TransIO ()
listen  port = do
       setSData $ Log False [] []
       sock <- liftIO $ withSocketsDo $ listenOn  port

       (h,host,port1) <- parallel $ Right <$> accept sock
                          `catch` (\(e::SomeException) -> sClose sock >> throw e)

       liftIO $  hSetBuffering h LineBuffering  -- !> "LISTEN in "++ show (h,host,port1)

       slog <- Transient $ liftIO $ (Just <$> hGetLine  h)
                          `catch` (\(e::SomeException) -> print "ERR" >>  return Nothing)

       setSData $ Connection port h sock  -- !> "setdata port=" ++ show port

       let log= read slog   -- !> "read1 " ++ slog
       setSData $ Log True log (reverse log)


-- | init a Transient process in a interactive as well as in a replay mode.
-- It is intended for twin processes that interact among them in different nodes.
beamInit :: PortID -> TransIO a -> IO b
beamInit port program=  keep $ do
    listen port   <|> return ()
    program
--    (program >> stop)   <|> close
--    where
--    close= do
--       Connection _ h sock <- getSData
--       liftIO $ hClose h  `catch` (\(e::SomeException) -> sClose sock)




instance Read PortNumber where
  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]


deriving instance Read PortID
deriving instance Typeable PortID

-- * Level 2: connections node lists and operations with the node list

type Node= (HostName,PortID)

nodeList :: TVar  [Node]
nodeList = unsafePerformIO $ newTVarIO []

deriving instance Ord PortID

getNodes :: TransIO [Node]
getNodes  = Transient $ Just <$> (liftIO $ atomically $ readTVar  nodeList)




addNodes   nodes= Transient . liftIO . atomically $ do
  prevnodes <- readTVar nodeList

  writeTVar nodeList $ nub $ prevnodes ++  nodes
  return $ Just ()

--getInterfaces :: TransIO TransIO HostName
--getInterfaces= do
--   host <- logged $ do
--      ifs <- liftIO $ getNetworkInterfaces
--      liftIO $ mapM_ (\(i,n) ->putStrLn $ show i ++ "\t"++  show (ipv4 n) ++ "\t"++name n)$ zip [0..] ifs
--      liftIO $ putStrLn "Select one: "
--      ind <-  input ( < length ifs)
--      return $ show . ipv4 $ ifs !! ind

--getInterfaces :: TransIO TransIO HostName
--getInterfaces= do
--   host <- logged $ do
--      ifs <- liftIO $ getNetworkInterfaces
--      liftIO $ mapM_ (\(i,n) ->putStrLn $ show i ++ "\t"++  show (ipv4 n) ++ "\t"++name n)$ zip [0..] ifs
--      liftIO $ putStrLn "Select one: "
--      ind <-  input ( < length ifs)
--      return $ show . ipv4 $ ifs !! ind


-- | execute a Transient action in each of the nodes connected. The results are mappend'ed
clustered :: (Typeable a, Show a, Read a) => Monoid a => TransIO a -> TransIO a
clustered proc= logged $ do
     nodes <- logged getNodes
     logged $ foldr (<>) mempty $ map (\(h,p) -> callTo h p proc) nodes !> "fold"

-- | Connect to a new node to another. The other node will notify about this connection to
-- all the nodes connected to him. the new connected node will receive the list of connected nodes
-- the nodes will be updated with this list. it can be retrieved with `getNodes`
connect ::   HostName ->  PortID ->  HostName ->  PortID -> TransientIO ()
connect host  port   remotehost remoteport=  do
    listen port <|> return ()
    logged $ do
        logged $ addNodes [(host,port)]
        logged $ liftIO $ putStrLn $ "connecting to: "++ show (remotehost,remoteport)
        host <- logged $ return host
        port <- logged $ return port
        nodes <- callTo remotehost remoteport   $ do
                   clustered $  addNodes [(host, port)]
                   getNodes

        logged $ addNodes nodes
        logged $ liftIO $ putStrLn $ "Connected to modes: " ++ show nodes


