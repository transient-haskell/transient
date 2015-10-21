test.hs{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Main where

import Network
import qualified Network.Socket as NS hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import  qualified Network.BSD as BSD


import System.IO hiding (hPutBufNonBlocking)
import Control.Concurrent
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Foreign.Ptr
import Foreign.Storable
import Data.ByteString.Internal
import Foreign.ForeignPtr.Safe



main = do


   let host= "localhost"; port= 2000
   forkIO $ listen' $ PortNumber port
   proto <- BSD.getProtocolNumber "tcp"
   bracketOnError
    (NS.socket NS.AF_INET NS.Stream proto)
    (sClose)  -- only done if there's an error
    (\sock -> do
       NS.setSocketOption sock NS.RecvBuffer 3000
       he <- BSD.getHostByName "localhost"
       NS.connect sock (NS.SockAddrInet port (BSD.hostAddress he))
       loop sock 0
       getChar)
       where
   loop sock x = do

      let msg =  BS.pack $ show x ++ "\n"
      let l = BS.length msg
      n <- send sock  msg
      when (n < l) $ do
         print $ "CONGESTION "++ show (l-n)
         sendAll sock $ BS.drop n msg

      loop sock (x +1)






listen'  port = do
   sock <- listenOn  port
   (h,host,port1) <- accept sock
   hSetBuffering h $ BlockBuffering Nothing
   repeatRead h
   where
   repeatRead h= do
       forkIO $ doit h
       return()
       where
       doit h= do
           s <- hGetLine h
           print s
           threadDelay 1000000
           doit h




