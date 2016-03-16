{-# LANGUAGE ScopedTypeVariables #-}


module Main where
import Transient.Base
import Transient.Move
import Transient.Logged
import Transient.DDS
import Control.Applicative
import Control.Monad.IO.Class
import Data.List
import Control.Exception
--import System.Environment
import System.Directory
import System.IO


import Network
import Network.HTTP
import           Data.ByteString                       (ByteString)
--import qualified Data.ByteString.Base64                as B64
import           Data.ByteString.Char8                 (unpack,pack)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Digest.Pure.SHA                  (bytestringDigest, sha1)
import Network.TCP
import Network.Stream (Result(..),fmapE,ConnError(..))
import Network.BufferType
main= keep $ do
   liftIO $ print "running"
   sock <- liftIO $  listenOn  $ PortNumber 2000

   SMore(h,_,_) <- parallel $ SMore <$> accept sock


   Right (method,uri, headers) <- liftIO $ receiveHTTPHead  h

   let identHead = filter (\ x -> case x of (Header(HdrCustom "Sec-WebSocket-Key:") _) -> True;_ ->False)
                   headers
   case identHead of
      [] -> liftIO $  hPutStrLn h $ show $ Response (2,0,0) "OK" []  wspage
      [Header _ ident] ->
       liftIO $ hPutStrLn h $ show $ Response (1,0,1) "Switching Protocols"
        [Header HdrUpgrade "websocket",
         Header HdrConnection "Upgrade",
         Header (HdrCustom "Sec-WebSocket-Accept") (unpack $ hashKey $ pack ident)]
         ""

receiveHTTPHead :: Handle -> IO (Result RequestData)
receiveHTTPHead conn= do
      fmapE (\es -> parseRequestHead (map (buf_toStr bufferOps) es))
            (readTillEmpty1 bufferOps (hGetLine' conn))

     where
     hGetLine' h= (Right <$> hGetLine h) `catch` excpt
     excpt (e :: SomeException) =  return $ Left(ErrorClosed)

hashKey :: ByteString -> ByteString
hashKey key = unlazy $ bytestringDigest $ sha1 $ lazy $ key `mappend` guid
  where
    guid = pack "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    lazy = BL.fromChunks . return
    unlazy = mconcat . BL.toChunks

wspage= "<!DOCTYPE html>\n\
\<meta charset=\"utf-8\" />\n\
\<title>WebSocket Test</title>\n\
\<script language=\"javascript\" type=\"text/javascript\">\n\
\  var wsUri = \"ws://localhost:2000/this\";\n\
\  var output;\n\
\  function init()\n\
\  {\n\
\    output = document.getElementById(\"output\");\n\
\    testWebSocket();\n\
\  }\n\
\  function testWebSocket()\n\
\  {\n\
\    websocket = new WebSocket(wsUri);\n\
\    websocket.onopen = function(evt) { onOpen(evt) };\n\
\    websocket.onclose = function(evt) { onClose(evt) };\n\
\    websocket.onmessage = function(evt) { onMessage(evt) };\n\
\    websocket.onerror = function(evt) { onError(evt) };\n\
\  }\n\
\  function onOpen(evt)\n\
\  {\n\
\    writeToScreen(\"CONNECTED\");\n\
\    doSend(\"WebSocket rocks\");\n\
\  }\n\
\  function onClose(evt)\n\
\  {\n\
\    writeToScreen(\"DISCONNECTED\");\n\
\  }\n\
\  function onMessage(evt)\n\
\  {\n\
\    writeToScreen('<span style=\"color: blue;\">RESPONSE: ' + evt.data+'</span>');\n\
\    websocket.close();\n\
\  }\n\
\  function onError(evt)\n\
\  {\n\
\    writeToScreen('<span style=\"color: red;\">ERROR:</span> ' + evt.data);\n\
\  }\n\
\  function doSend(message)\n\
\  {\n\
\    writeToScreen(\"SENT: \" + message);\n\
\    websocket.send(message);\n\
\  }\n\
\  function writeToScreen(message)\n\
\  {\n\
\    var pre = document.createElement(\"p\");\n\
\    pre.style.wordWrap = \"break-word\";\n\
\    pre.innerHTML = message;\n\
\    output.appendChild(pre);\n\
\  }\n\
\  window.addEventListener(\"load\", init, false);\n\
\</script>\n\
\<h2>WebSocket Test</h2>\n\
\<div id=\"output\"></div>\n"
