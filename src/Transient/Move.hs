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
--           liftIO $ print "remote"
           r <- remoteProc
           (h,sock) <- getSData
           liftIO $ hPutStr h (show r)  `catch` (\(e::SomeException) -> sClose sock)
           stop

         else  async $ do -- local side
--           liftIO $   print "local"
           h <-  connectTo node port
           hPutStrLn h (show $ reverse log) >> hFlush h
--           liftIO $ print "sent"
           s <- hGetContents h
--           let l= length s
--           l `seq` liftIO (print s)
           return $ read s




listen  port = do
--       liftIO $ print "listen"
       sock <- liftIO $ listenOn  port

       (h,_,_) <- spawn $ accept sock
       liftIO $ hSetBuffering h LineBuffering
--       liftIO $ print "received something"
       slog <- liftIO $ hGetLine  h
----       liftIO $ putStrLn $ "Listen rec=" ++ show (length slog) ++ slog
       setSData (h,sock)

       setSData $ Log True $ read slog





beamInit port program=  keep $ do
    listen port  <|> return ()
    (program >> empty)  <|> close
    where
    close= do
       (h,sock) <- getSData
       liftIO $ hClose h  `catch` (\(e::SomeException) -> sClose sock)


