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
    ,ScopedTypeVariables #-}
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
import Control.Concurrent.MVar
import Data.Monoid

data IDynamic= IDyns String | forall a.(Read a, Show a,Typeable a) => IDynamic a

instance Show IDynamic where
  show (IDynamic x)= show $ show x
  show (IDyns s)= s

instance Read IDynamic where
  readsPrec n str= map (\(x,s) -> (IDyns x,s)) $ readsPrec n str



fromIDyn :: (Read a, Show a, Typeable a) => IDynamic -> a
fromIDyn (IDynamic x)= unsafeCoerce x

fromIDyn (IDyns s)= read s !> "read"

toIDyn x= IDynamic x

type Recover= Bool
data Log= Log Recover [IDynamic] deriving Typeable

step :: (Show a, Read a, Typeable a) => TransientIO a -> TransientIO a
step mx= do

    Log recover rs <- getSData <|> return (Log False [])

    if recover && not (null rs)
      then do
        setSData . Log recover $ tail rs
        return $ fromIDyn $ head rs
      else do
        r <- mx
        setSData . Log False $ toIDyn r:rs
        return r

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
  Log rec log <- getSData <|> return (Log False [])
  if rec then return () else do
      h <- liftIO $ connectTo node port
      liftIO $ hSetBuffering h LineBuffering
      liftIO $ hPutStrLn h (show $ reverse log)  >> hFlush h
      liftIO $ hClose h
      delSData h
      stop

forkTo  :: HostName -> PortID -> TransientIO ()
forkTo node port= do
  Log rec log <- getSData <|> return (Log False [])
  if rec then return () else do
      h <- liftIO $ connectTo node port
      liftIO $ hSetBuffering h LineBuffering
      liftIO $ hPutStrLn h (show $ reverse log)  >> hFlush h
      liftIO $ hClose h
      delSData h

callTo :: (Show a, Read a) => HostName -> PortID -> TransientIO a -> TransientIO a
callTo node port remoteProc= do
      Log rec log <- getSData <|> return (Log False [])
      if rec
         then do -- remote side

           r <- remoteProc
           (h,sock) <- getSData
           liftIO $ hPutStrLn h (show r)  `catch` (\(e::SomeException) -> sClose sock)
           stop

         else  async $ do -- local side

           h <- connectTo node port
           hPutStrLn h (show $ reverse log) >> hFlush h
           liftIO $ hSetBuffering h LineBuffering
           s <- hGetLine h
           hClose h

           return $ read s  -- !> "read: " ++ s




listen  port = do
--       liftIO $ print "listen"
       sock <- liftIO $ listenOn  port

       (h,_,_) <- parallel $ Right <$> accept sock
       liftIO $ hSetBuffering h LineBuffering
--       liftIO $ print "received something"
       slog <- liftIO $ hGetLine  h
----       liftIO $ putStrLn $ "Listen rec=" ++ show (length slog) ++ slog
       setSData (h,sock)

       setSData $ Log True $ read slog

beamInit :: PortID -> Transient StateIO a -> IO b
beamInit port program=  keep $ do
    listen port  <|> return ()
    (program >> empty)  <|> close
    where
    close= do
       (h,sock) <- getSData
       liftIO $ hClose h  `catch` (\(e::SomeException) -> sClose sock)

-- longCallTo: for very long processes that return the result to the calling method




type Node= (HostName,PortID)

nodeList = unsafePerformIO $ newMVar ([] :: [Node])

--addNode :: HostName -> PortID -> Integer -> TransientIO ()
--addNode  remotehost remoteport port= do
--   ifs <- liftIO $ getNetworkInterfaces
--
--   ind <-  foldl (<|>) empty $ map (\(i,n) -> option i $ show (ipv4 n) ++ "\t"++name n)  $ zip [1..] ifs
--   let host = show . ipv4 $ ifs !! ind
--
--   callTo remotehost remoteport $ do
--       nodes <- liftIO $ readMVar nodeList
--       map (\(h,p) -> addNode' h p host port) $ (host, PortNumber $ fromInteger port): nodes
--
--
--   where
--   addNode' h p h' p'= callTo h p . liftIO $ do
--       withMVar nodeList $ \nodes -> return  ((h', PortNumber $ fromInteger p'): nodes)
--       return ()








