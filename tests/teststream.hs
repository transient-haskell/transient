{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Main where

import Network
import qualified Network.Socket as NS
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

import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import GHC.IO.Buffer
import GHC.IO.BufferedIO as Buffered
import GHC.IO.Device as RawIO
import GHC.IO.FD
import GHC.Word
import Data.IORef
import Data.Typeable
import System.IO.Unsafe
import Data.Monoid

main = do

   let port= PortNumber 2000

   forkIO $ listen' port
   h <- connectTo' "localhost" port
   liftIO $  hSetBuffering h $  BlockBuffering Nothing
   loop h 0
   getChar
   where
   loop h x = hPutStrLn' h (show x) >>  loop h (x +1)

   hPutStrLn' h str= do
       let bs@(PS ps s l) = BS.pack $ str  ++ "\n"
       n <- withForeignPtr ps $ \p-> hPutBufNonBlocking h (p `plusPtr` s) l
       when( n < l) $ do
          print (n,l)
          print "BUFFER FULLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL"
          hFlush h
          print "AFTER BUFFER FLUSHHHH"
          withForeignPtr ps $ \p -> hPutBuf  h ( p `plusPtr` (n * sizeOf  'x' ) )  (l - n)
          print "AFTER HPUTBUF"
          return ()

connectTo' hostname (PortNumber port) = do
    proto <- BSD.getProtocolNumber "tcp"
    bracketOnError
        (NS.socket NS.AF_INET NS.Stream proto)
        (sClose)  -- only done if there's an error
        (\sock -> do
          NS.setSocketOption sock NS.SendBuffer 300
          he <- BSD.getHostByName hostname
          NS.connect sock (NS.SockAddrInet port (BSD.hostAddress he))

          NS.socketToHandle sock ReadWriteMode
        )

hPutBufNonBlocking handle ptr count
  | count == 0 = return 0
  | count <  0 = error "negative chunk size"
  | otherwise =
    wantWritableHandle "hPutBuf" handle $
      \ h_@Handle__{..} -> bufWriteNonBlocking h_ (castPtr ptr) count False



bufWriteNonBlocking :: Handle__-> Ptr Word8 -> Int -> Bool -> IO Int
bufWriteNonBlocking h_@Handle__{..} ptr count can_block =
  seq count $ do  -- strictness hack
  old_buf@Buffer{  bufR=w, bufSize=size }  <- readIORef haByteBuffer
  -- print (size,w, count)
  old_buf'@Buffer{  bufR=w', bufSize = size' } <-
        if size - w <= count
          then   do
            (written,old_buf') <- Buffered.flushWriteBuffer0 haDevice old_buf
            writeIORef haByteBuffer old_buf'
            print (size , written,w, count)
            print (bufSize old_buf', bufR old_buf')
            return old_buf'
          else return old_buf

  let count'= if size' - w' > count then count else size' - w'
  writeChunkNonBlocking h_ (castPtr ptr) count'
  writeIORef haByteBuffer old_buf'{ bufR = w' + count' }

  return count'



writeChunkNonBlocking h_@Handle__{..} ptr bytes
  | Just fd <- cast haDevice  =  RawIO.writeNonBlocking (fd::FD) ptr bytes
  | otherwise = error "Todo: hPutBuf"




listen'  port = do
   sock <- withSocketsDo $ listenOn  port
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
           -- print s
           --threadDelay 10
           doit h




