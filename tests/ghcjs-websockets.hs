
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, DeriveDataTypeable,
             UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes,
             MagicHash, OverloadedStrings
  #-}
import  JavaScript.Web.WebSocket
import JavaScript.Web.MessageEvent
import           Data.JSString (JSString)



main :: IO ()
main = do
  wsloc <- wslocation
  print wsloc
  ws <- connect WebSocketRequest
          { url       = wsloc -- "ws://localhost:2000"
          , protocols = ["chat"]
          , onClose   = Just $ const $ return() -- Maybe (CloseEvent -> IO ()) -- ^ called when the connection closes (at most once)
          , onMessage = Just recMessage -- Maybe (MessageEvent -> IO ()) -- ^ called for each message
          }
  print "CONEXION REALIZADA"
  send "HELLOHELLOHELLOHELLOHELLOHELLO" ws

recMessage e= -- print "SOMething HAS BEEN RECEIVED"
 do
  let d = getData e
  case d of
    StringData str  -> putStrLn "RECEIVED " >> print str
    BlobData   blob -> error " blob"
    ArrayBufferData arrBuffer -> error "arrBuffer"


foreign import javascript unsafe
        "var loc = window.location, new_uri;\
        \if (loc.protocol === \"https:\") {\
        \    new_uri = \"wss:\";\
        \} else {\
        \    new_uri = \"ws:\";\
        \}\
        \new_uri += \"//\" + loc.host;\
        \new_uri += loc.pathname;\
        \$r = new_uri"
     wslocation :: IO JSString



