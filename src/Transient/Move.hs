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
{-# LANGUAGE DeriveDataTypeable , ExistentialQuantification, OverloadedStrings
    ,ScopedTypeVariables, StandaloneDeriving, RecordWildCards, FlexibleContexts, CPP
    ,GeneralizedNewtypeDeriving #-}
module Transient.Move where
import Transient.Base hiding (stop)
import Transient.Logged
import Transient.EVars
import Transient.Stream.Resource
import Data.Typeable
import Control.Applicative
#ifndef ghcjs_HOST_OS
import Network
import Network.Info
import qualified Network.Socket as NS
import qualified Network.BSD as BSD
import qualified Network.WebSockets as NS(sendTextData,receiveData, Connection,RequestHead(..),sendClose)
import qualified Network.WebSockets.Connection   as WS
import Network.WebSockets.Stream   hiding(parse)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Network.Socket.ByteString as SBS(send,sendMany,recv)
import Data.CaseInsensitive(mk)
import Data.Char(isSpace)
#else
import  JavaScript.Web.WebSocket
import  qualified JavaScript.Web.MessageEvent as JM
import GHCJS.Prim (JSVal)
import qualified Data.JSString as JS


import           JavaScript.Web.MessageEvent.Internal
import           GHCJS.Foreign.Callback.Internal (Callback(..))
import qualified GHCJS.Foreign.Callback          as CB
#endif


import Control.Monad.State
import System.IO
import Control.Exception
import Data.Maybe
import Unsafe.Coerce

--import System.Directory
import Control.Monad

import System.IO.Unsafe
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar

import Data.Monoid
import qualified Data.Map as M
import Data.List (nub,(\\),find)
import Data.IORef


import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO

import Control.Concurrent





import Data.Dynamic
import Data.String

#ifdef ghcjs_HOST_OS
type HostName  = String
newtype PortID = PortNumber Int deriving (Read, Show, Eq, Typeable)
#endif
data Node= Node{ nodeHost   :: HostName
               , nodePort   :: PortID
               , connection :: IORef Pool
               , services   :: [Service]}
         | WebNode{wconnection:: IORef Pool}
         deriving Typeable


newtype Cloud a= Cloud (TransIO a) deriving (Functor, Applicative, Alternative, Monad, MonadState EventF, Monoid)

local :: Loggable a => TransIO a -> Cloud a
local =  Cloud . logged


runCloud :: Cloud a -> TransIO a
runCloud (Cloud mx)= mx

#ifndef ghcjs_HOST_OS
runCloud' :: Cloud a -> IO a
runCloud' (Cloud mx)= keep mx
#endif


onAll ::  TransIO a -> Cloud a
onAll =  Cloud

loggedc (Cloud mx)= Cloud $ logged mx

lliftIO :: Loggable a => IO a -> Cloud a
lliftIO= local . liftIO

--remote :: Loggable a => TransIO a -> Cloud a
--remote x= Cloud $ step' x $ \full x ->  Transient $ do
--            let add= Wormhole: full
--            setSData $ Log False add add
--
--            r <-  runTrans x
--
--            let add= WaitRemote: full
--            (setSData $ Log False add add)     -- !!> "AFTER STEP"
--            return  r

---- | stop the current computation
--stop :: Cloud a
--stop= empty

-- | continue the execution in a new node
-- all the previous actions from `listen` to this statement must have been logged
beamTo :: Node -> Cloud ()
beamTo node =  do
  Log rec log _ <- onAll getSData <|> return (Log False [][])
  if rec then return () else onAll $ do
      msendToNode node $ SLast $ reverse log
      let log'= Wait: log
      setSData $ Log rec log' log'
      empty


-- | execute in the remote node a process with the same execution state
-- all the previous actions from `listen` to this statement must have been logged
forkTo  :: Node -> Cloud ()
forkTo node= do
  Log rec log _<- onAll getSData <|> return (Log False [][])
  if rec then return () else onAll $ do
      msendToNode node $ SLast $ reverse log
      let log'= Wait: log
      setSData $ Log rec log' log'

-- | executes an action in another node.
-- All the previous actions from `listen` to this statement must have been logged
callTo :: Loggable a => Node -> Cloud a -> Cloud a
callTo n p =  do
    SLast r <- streamFrom n (SLast <$> p)
    return r


-- | synonymous of `callTo`
-- all the previous actions from `listen` to this statement must have been logged

runAt :: Loggable a => Node -> Cloud a -> Cloud a
runAt= callTo


msendToNode node msg= do
      conn <-  mconnect  node
      liftIO $ msend conn msg


msend :: Loggable a => Connection -> StreamData a -> IO ()


#ifndef ghcjs_HOST_OS

msend (Connection _(Just (Node2Node _ h sock)) _ _ blocked _ _ ) r= liftIO $ do
  withMVar blocked $
             const $ do
                     hPutStrLn   h "LOG a b"
                     hPutStrLn   h ""
                     hPutStrLn h (show r)
                     hFlush h                         -- !!> show ("sending: ", r)
--             `catch` (\(e::SomeException) -> sClose sock)


msend (Connection _(Just (Node2Web sconn)) _ _ blocked _ _) r=
  withMVar blocked $ const $ NS.sendTextData sconn $ BS.pack (show r)


#else

msend (Connection _ (Just (Web2Node sconn)) _ _ blocked _ _) r=
  withMVar blocked $ const $ JavaScript.Web.WebSocket.send  (JS.pack $ show r) sconn   -- !!> "MSEND SOCKET"



#endif

msend (Connection _ Nothing _ _  _ _ _ ) _= error "calling msend:  with no connection"

mread :: Loggable a => Connection -> TransIO (StreamData a)
#ifdef ghcjs_HOST_OS


mread (Connection _ (Just (Web2Node sconn)) _ _ _ _ _)=  wsRead sconn


--wsAsk ws tosend= do
--   liftIO $ send ws tosend
--   wsRead ws


wsRead :: Loggable a => WebSocket  -> TransIO  a
wsRead ws= do
  dat <- react (hsonmessage ws) (return ())
  case JM.getData dat of
    JM.StringData str  ->  return (read $ JS.unpack str)  --  !!> ("WSREAD RECEIVED " ++ show str)
    JM.BlobData   blob -> error " blob"
    JM.ArrayBufferData arrBuffer -> error "arrBuffer"

{-
wsRead1 :: Loggable a => WebSocket  -> TransIO (StreamData a)
wsRead1 ws= do
  reactStream (makeCallback MessageEvent) (js_onmessage ws) CB.releaseCallback (return ())
  where
  reactStream createHandler setHandler removeHandler iob= Transient $ do
        cont    <- getCont
        hand <- liftIO . createHandler $ \dat ->do
              runStateT (setSData dat >> runCont cont) cont
              iob
        mEvData <- getSessionData
        case mEvData of
          Nothing -> liftIO $ do
                        setHandler hand
                        return Nothing

          Just dat -> do
             liftIO $ print "callback called 2*****"
             delSessionData dat
             dat' <- case getData dat of
                 StringData str  -> liftIO $ putStrLn "WSREAD RECEIVED " >> print str >> return (read $ JS.unpack str)
                 BlobData   blob -> error " blob"
                 ArrayBufferData arrBuffer -> error "arrBuffer"
             liftIO $ case dat' of
               SDone -> do
                        removeHandler $ Callback hand
                        empty
               sl@(SLast x) -> do
                        removeHandler $ Callback hand     -- !!> "REMOVEHANDLER"
                        return $ Just sl
               SError e -> do
                        removeHandler $ Callback hand
                        print e
                        empty
               more -> return (Just  more)
-}


wsOpen :: JS.JSString -> TransIO WebSocket
wsOpen url= do
   ws <-  liftIO $ js_createDefault url      --  !> ("wsopen",url)
   react (hsopen ws) (return ())             -- !!> "react"
   return ws                                 -- !!> "AFTER ReACT"



foreign import javascript safe
    "$1.onmessage =$2;"
   js_onmessage :: WebSocket  -> JSVal  -> IO ()

hsonmessage ::WebSocket -> (MessageEvent ->IO()) -> IO ()
hsonmessage ws hscb= do
  cb <- makeCallback MessageEvent hscb
  js_onmessage ws cb

foreign import javascript safe
             "$1.onopen =$2;"
   js_open :: WebSocket  -> JSVal  -> IO ()

newtype OpenEvent = OpenEvent JSVal deriving Typeable
hsopen ::  WebSocket -> (OpenEvent ->IO()) -> IO ()
hsopen ws hscb= do
   cb <- makeCallback OpenEvent hscb
   js_open ws cb

makeCallback :: (JSVal -> a) ->  (a -> IO ()) -> IO JSVal

makeCallback f g = do
   Callback cb <- CB.syncCallback1 CB.ContinueAsync (g . f)
   return cb


foreign import javascript safe
   "new WebSocket($1)" js_createDefault :: JS.JSString -> IO WebSocket


#else
mread (Connection _(Just (Node2Node _ h _)) _ _ blocked _ _ ) =
       parallel $ do
              hGetLine h       -- to skip LOG header
              hGetLine h
              readHandler  h


mread (Connection node  (Just (Node2Web sconn )) bufSize events blocked _ _ )=
        parallel $ do
            s <- NS.receiveData sconn
            return . read $  BS.unpack s         -- !!> show ("WS MREAD RECEIVED ---->", s)

#endif



wormhole :: Loggable a => Node -> Cloud a -> Cloud a
wormhole node (Cloud comp) = local $ Transient $ do
   moldconn <- getData --`onNothing` error "wormhole: no connection set"

   Log rec log fulLog <- getData `onNothing` return (Log False [][])
--   initState
   let lengthLog= length fulLog
   if not rec                           -- !!> show rec
            then runTrans $ do
                conn <- mconnect node  -- !!> ("connecting node " ++ show node)           -- !!> "wormhole local"

                liftIO $ msend conn $ SLast $ reverse fulLog   -- !!> ("sending "++ show fulLog) -- SLast will disengage  the previous wormhole/listen

                setSData $ conn{calling= True,offset= lengthLog} -- WormHole conn True  lengthLog
                (mread conn >>= check fulLog) <|> return ()   -- !!> "MREAD"
--                putState    !!> "PUTSTATE"
                r <- comp
                when (isJust moldconn) $ setSData (fromJust moldconn)
                return r


            else do
               let oldconn = fromMaybe (error "wormhole: no connection in remote node") moldconn
--             if null log    -- has recovered state already

--              then do
               setData $ oldconn{calling= False,offset= lengthLog}

               runTrans $ do
                  mlog <- mread oldconn    -- !!> "MREAD"
                  check  fulLog mlog
                  r <- comp
                  setSData  oldconn
                  setSData WasRemote
                  return r

--              else do
--                  setData $ oldconn{calling= False,offset= lengthLog}
--                  r <- runTrans comp                 --  !!> "????????????????"
--                  setSData oldconn
--                  return r

  where
--  initState= do
--       rstat <-liftIO $ newIORef undefined
--       setSData rstat
--       stat <- gets mfData
--       liftIO $ writeIORef rstat stat
--
--  putState = do
--        rstate <- getSData <|> error "rstate not defined" :: TransIO (IORef(M.Map TypeRep SData))
--        st <- get
--        log@(Log _ _ l) <- getSData :: TransIO Log
--        con <-getSData :: TransIO Connection
--        mfDat <- liftIO $ readIORef rstate -- !!> show ("LOG BEFORe",  l)
--        put st {mfData= mfDat}
--        setSData log
--        setSData con
--




  check fulLog mlog =
   case  mlog  of          -- !!> "RECEIVED "++ show mlog  of
             SError e -> do
                 finish $ Just e
                 empty

             SDone -> finish Nothing >> empty
             SMore log -> setSData (Log True log $ reverse log ++  fulLog ) -- !!> ("SETTING "++ show log)
             SLast log -> setSData (Log True log $ reverse log ++  fulLog ) -- !!> ("SETTING "++ show log)



--teleport1 wh= do
--  local ::do

--     send log desde cut a teleport  -- lo lee readHandler (1)
--     set log a true -- ahora es nodo remoto
--     stop
--
--  remoto
--     set log a false  -- ahora el remoto es local
--


teleport :: Cloud ()
teleport =  local $ Transient $ do
    conn@Connection{calling= calling,offset= n} <- getData
           `onNothing` error "teleport: No connection defined: use wormhole"
    Log rec log fulLog <- getData `onNothing` return (Log False [][])    -- !!> "TELEPORT"
    if not rec
      then  do
         liftIO $ msend conn $ SMore $ drop n $ reverse fulLog
              -- !!> ("TELEPORT LOCAL sending" ++ show (drop n $ reverse fulLog))
                 -- will be read by wormhole remote
         when (not calling) $ setData WasRemote
--         getState   !!> "GETSTAT"
         return Nothing
      else do  delSData WasRemote

               return (Just ())   -- !!> "TELEPORT remote"
--   where
--   getState = do
--        rstate <- getData `onNothing` error "rstate not defined"
--        stnew <- get
--        liftIO $ writeIORef rstate $ mfData stnew


-- | copy session data variable from the local to the remote node.
-- The parameter is the default value if there is none set in the local node.
-- Then the default value is also set in the local node.
copyData def = do
  r <- local getSData <|> return def
  onAll $ setSData r
  return r

streamFrom :: Loggable a => Node -> Cloud (StreamData a) -> Cloud  (StreamData a)
streamFrom node  remoteProc= wormhole node $ do

       teleport     -- !!> "TELEPORT 1"

       r <-  loggedc remoteProc

       teleport     -- !!> "TELEPORT 2"

       return r


-- | `callTo` can stream data but can not inform the receiving process about the finalization. This call
-- does it.
--
{- All the previous actions from `listen` to this statement must have been logged
streamFrom1 :: Loggable a => Node -> TransIO (StreamData a) -> TransIO  a -- (StreamData a)
streamFrom1 node remoteProc= logged $ Transient $ do
      liftIO $ print "STREAMFROM"
      Log rec log fulLog <- getData `onNothing` return (Log False [][])
      if rec
         then
          runTrans $ do
            liftIO $ print "callTo Remote executing"
            conn <- getSData  <|> error "callTo receive: no connection data"

            r <- remoteProc                  -- !> "executing remoteProc" !> "CALLTO REMOTE" -- LOg="++ show fulLog
            n <- liftIO $ msend conn  r      -- !> "sent response"
            setSData WasRemote
            stop
          <|> do
            setSData WasRemote
            stop

         else  do
            cont <- getCont
            runTrans $ process (return()) (mconnect node) (mcloseRelease cont node) $ \conn _ -> do

                liftIO $ msend conn  (SLast $ reverse fulLog)  !> "CALLTO LOCAL" -- send "++ show  log

                let log'= Wait:tail log
                setData $ Log rec log' log'
                liftIO $ print "mread in callTO"
                mread conn

      where
      mcloseRelease :: EventF -> Node -> Connection -> Maybe SomeException -> IO ()
      mcloseRelease cont node conn reason=
         case reason of
            Nothing -> release node conn
            Just r -> do
              forkIO $ mclose conn

              killChildren cont
-}
   {-
         runTrans $ do
            conn <-  mconnect  node !> "mconnect"
            onFinish $ \_ -> do
                   liftIO $ print "MCLOSE"
                   liftIO $ mclose conn
                   c <- getCont
                   liftIO $ killChildren c -- liftIO $ myThreadId >>= \th -> liftIO (print th) >> killThread th


            liftIO $ msend conn  (SLast $ reverse fulLog)  !> "CALLTO LOCAL" -- send "++ show  log

            let log'= Wait:tail log
            setData $ Log rec log' log'
            liftIO $ print "mread in callTO"
            r <- mread conn
--              adjustRecThreads h

            case r of
                 SError e -> do
                     liftIO $ do
                        release node conn
                        print e
                     stop
                 SDone ->  release node conn >> empty
                 smore@(SMore x) -> return smore
                 other ->  release node conn >> return other

-}
--      where
--      adjustRecThreads h= do
--          b <- liftIO $ hWaitForInput  h 1
--          addThreads' $ if b then 1 else 0
--          liftIO $ putStrLn $ "REC "++ show (case b of True -> "INC" ; _ -> "DEC")999999*
--
--      adjustSenderThreads n
--         | n > 2 = addThreads' (-1)  >> liftIO (putStrLn ("SEND DEC"))
--         | n==0 = addThreads' 1  >> liftIO (putStrLn ("SEND INC"))
--         | otherwise= return () >> liftIO(myThreadId >>= \th -> (putStrLn ("SEND "++ show th)))

release (Node h p rpool _) hand= liftIO $ do
--    print "RELEASED"
    atomicModifyIORef rpool $  \ hs -> (hand:hs,())
      -- !!> "RELEASED"

mclose :: Connection -> IO ()
#ifndef ghcjs_HOST_OS
mclose (Connection _  (Just (Node2Node _ h sock )) _ _ _ _ _ )= hClose h  -- !!> "Handle closed"
mclose (Connection node  (Just (Node2Web sconn )) bufSize events blocked _ _ )= NS.sendClose sconn ("closemsg" ::ByteString)
#else
mclose (Connection _ (Just (Web2Node sconn)) _ _ blocked _ _)= JavaScript.Web.WebSocket.close Nothing Nothing sconn
#endif

mconnect :: Node -> TransIO Connection
mconnect  (Node host port  pool _)=  do
    mh <- liftIO $ atomicModifyIORef pool $ \mh ->
      case mh of
       (handle:hs) -> (hs,Just handle)
       [] -> ([],Nothing)
    case mh of
      Just handle ->
          return handle   -- !!>   "REUSED!"

      Nothing ->

#ifndef ghcjs_HOST_OS
        liftIO $ do
          let size=8192
          h <-  connectTo' size host port  -- !!> ("CONNECTING "++ show port)
          hSetBuffering h $ BlockBuffering $ Just size
          let conn= (defConnection 8100){connData= Just $ Node2Node u h u}
--          writeIORef pool [conn]
          return conn
#else
        do
          ws <- connectToWS host port
          let conn= (defConnection 8100){myNode=(),connData= Just $ Web2Node ws}
--          liftIO $ writeIORef pool [conn]
          return conn
#endif
  where u= undefined

mconnect _ = empty



#ifndef ghcjs_HOST_OS
connectTo' bufSize hostname (PortNumber port) =  do
        proto <- BSD.getProtocolNumber "tcp"
        bracketOnError
            (NS.socket NS.AF_INET NS.Stream proto)
            (sClose)  -- only done if there's an error
            (\sock -> do
              NS.setSocketOption sock NS.RecvBuffer bufSize
              NS.setSocketOption sock NS.SendBuffer bufSize
              he <- BSD.getHostByName hostname
              NS.connect sock (NS.SockAddrInet port (BSD.hostAddress he))

              NS.socketToHandle sock ReadWriteMode
            )
#else
connectToWS  h (PortNumber p) =
   wsOpen $ JS.pack $ "ws://"++ h++ ":"++ show p
#endif
#ifndef ghcjs_HOST_OS
-- | A connectionless version of callTo for long running remote calls
-- myNode should be set with `setMyNode`
callTo' :: (Show a, Read a,Typeable a) => Node -> Cloud a -> Cloud a
callTo' node remoteProc=  do
    mynode <-  getMyNode
    beamTo node
    r <-  remoteProc
    beamTo mynode
    return r
#endif

type Blocked= MVar ()
type BuffSize = Int
data ConnectionData=
#ifndef ghcjs_HOST_OS
                   Node2Node{port :: PortID
                              ,handle :: Handle
                              ,socket ::Socket
                                   }

                   | Node2Web{webSocket :: NS.Connection}
#else

                   Web2Node{webSocket :: WebSocket}
#endif


#ifndef ghcjs_HOST_OS
data Connection= Connection{myNode :: TVar  MyNode
#else
data Connection= Connection{myNode :: ()
#endif
                           ,connData :: (Maybe(ConnectionData))
                           ,bufferSize ::BuffSize
                           ,comEvent :: EVar Dynamic
                           ,blocked :: Blocked
                           ,calling :: Bool
                           ,offset  :: Int}
                  deriving Typeable

-- | Updates the mailbox of another node. It means that the data is transported trough the network
-- The call does not end until the mailbox entry is porcessed in the remote node.
-- Mailboxes are node-wide, for all processes that share the same connection data, that is, are under the
-- same `listen`  or `connect`
-- while EVars scope are visible for the process where they are initialized and their children.
-- Internally a mailbox is a well known EVar stored by `listen` in the `Connection` state.
remotePutMailBox :: Loggable a => Node -> a -> Cloud ()
remotePutMailBox node dat= runAt node $ putMailBox dat

-- | updates the local mailbox.
putMailBox :: Typeable a => a -> Cloud ()
putMailBox dat= local $ Transient $ do
   Connection{comEvent=comEvent}<- getData
      `onNothing` error "accessing network events out of listen"
   runTrans $ writeEVar comEvent $ toDyn dat

-- | wait until a message of the type expected appears in the mailbox. Then executes the continuation
-- When the message appears, all the waiting `getMailBox` are executed from newer to the older
-- following the `readEVar` order.
getMailBox :: Loggable a => Cloud a
getMailBox = local $ Transient $ do
   Connection{comEvent=comEvent} <- getData
       `onNothing` error "accessing network events out of listen"
   d <- runTrans $ readEVar comEvent
   return $ cast d


defConnection :: Int -> Connection

#ifndef ghcjs_HOST_OS
defConnection size=
 Connection (unsafePerformIO $  newTVarIO $ MyNode  $ createNode "invalid" 0) Nothing  size
                 (error "accessing network events out of listen")
                 (unsafePerformIO $ newMVar ())
                 False 0
#else
defConnection size= Connection () Nothing  size
                 (error "accessing network events out of listen")
                 (unsafePerformIO $ newMVar ())
                 False 0
#endif


#ifndef ghcjs_HOST_OS
setBufSize :: Int -> TransIO ()
setBufSize size= Transient $ do
   conn<- getData `onNothing` return (defConnection 8192)
   setData $ conn{bufferSize= size}
   return $ Just ()

getBuffSize=
  (do getSData >>= return . bufferSize) <|> return  8192

readHandler h= do
    line <- hGetLine h

    let [(v,left)]= readsPrec 0 line

    return  v     -- !!> (show $ typeOf v)

  `catch` (\(e::SomeException) -> return $ SError   e)
--   where
--   hGetLine' h= do






--listen ::  Node ->  TransIO ()
--listen  (node@(Node _  port _ _)) = do
--   addThreads 1
--   setMyNode node
--   setSData $ Log False [] []
--
--   Connection node  _ bufSize events blocked <- getSData <|> return (defConnection 8192)
--
--   sock <- liftIO $  listenOn  port
--   liftIO $ do NS.setSocketOption sock NS.RecvBuffer bufSize
--               NS.setSocketOption sock NS.SendBuffer bufSize
--   SMore(h,host,port1) <- parallel $ (SMore <$> accept sock)
--                          `catch` (\(e::SomeException) -> do
--                               print "socket exception"
--                               sClose sock
--                               return SDone)
--
--
--   setSData $ Connection node  (Just (Node2Node port h sock )) bufSize events blocked
--
--   liftIO $  hSetBuffering h LineBuffering -- !> "LISTEN in "++ show (h,host,port1)
--
--   mlog <- parallel $ readHandler h
--
--   case  mlog  of
--         SError e -> do
--             liftIO $ do
--                hClose h
--                putStr "listen: "
--                print e
--             stop
--
--         SDone -> liftIO (hClose h) >> stop
--         SMore log -> setSData $ Log True log (reverse log)
--         SLast log -> setSData $ Log True log (reverse log)

listen ::  Node ->  Cloud ()
listen  (node@(Node _  (PortNumber port) _ _)) = onAll $ do
   addThreads 1
   setMyNode node
   setSData $ Log False [] []

   Connection node  _ bufSize events blocked _ _ <- getSData <|> return (defConnection 8192)

   sock <- liftIO . listenOn  $ PortNumber port

   liftIO $ do NS.setSocketOption sock NS.RecvBuffer bufSize
               NS.setSocketOption sock NS.SendBuffer bufSize


   (conn,_) <- waitEvents $  NS.accept sock         -- !!> "BEFORE ACCEPT"


   h <- liftIO $ NS.socketToHandle conn ReadWriteMode     -- !!> "NEW SOCKET CONNECTION"

--   let conn= Connection node  (Just (Node2Node port h sock )) bufSize events blocked
--   setSData conn
--   mlog <- mread conn

   (method,uri, headers) <- receiveHTTPHead h
--   liftIO $ print ("RECEIVED ---------->",method,uri, headers)
   mlog <- case method of
     "LOG" ->
          do
           setSData $ (Connection node  (Just (Node2Node (PortNumber port) h sock ))
                         bufSize events blocked False 0  :: Connection)
           parallel $ readHandler  h       -- !!> "read Listen"  -- :: TransIO (StreamData [LogElem])
     _ -> do
           sconn <- httpMode (method,uri, headers) conn
           setSData $ (Connection node  (Just (Node2Web sconn ))
                         bufSize events blocked False 0 :: Connection)

           parallel $ do
               msg <- WS.receiveData sconn
--               liftIO $ print ("WS RECEIVED: ", msg)
               return . read $ BC.unpack msg

--   liftIO $ putStr "LISTEN RECEIVED " >> print mlog

   case  mlog  of
             SError e -> do
                 liftIO $ do
                    hClose h
                    putStr "listen: "
                    print e
--                 killChilds
                 c <- getCont
                 liftIO $ killChildren . fromJust $ parent c
                 empty

             SDone ->  empty -- liftIO (hClose h) >> stop
             SMore log -> setSData $ Log True log (reverse log)
             SLast log -> setSData $ Log True log (reverse log)
--   liftIO $ print "END LISTEN"



-- | init a Transient process in a interactive as well as in a replay mode.
-- It is intended for twin processes that interact among them in different nodes.
beamInit :: Node  -> Cloud a -> IO a
beamInit  node  program =   keep transio
  where
  Cloud transio=  do
    listen  node   <|> return ()
    program



instance Read PortNumber where
  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]


deriving instance Read PortID
deriving instance Typeable PortID
#endif


type Pool= [Connection] --  Pool{free :: [Handle], pending :: Int}
type Package= String
type Program= String
type Service= (Package, Program, Int)






-- * Level 2: connections node lists and operations with the node list


{-# NOINLINE emptyPool #-}
emptyPool :: MonadIO m => m (IORef Pool)
emptyPool= liftIO $ newIORef  []

createNode :: HostName -> Integer -> Node
createNode h p= Node h ( PortNumber $ fromInteger p) (unsafePerformIO emptyPool) []

createWebNode= WebNode  (unsafePerformIO emptyPool)

instance Eq Node where
    Node h p _ _ ==Node h' p' _ _= h==h' && p==p'
    _ == _ = False

instance Show Node where
   show (Node h p _ servs)= show (h,p,servs)
   show (WebNode _)= "webnode"

instance Read Node where
     readsPrec _ ('w':'e':'b':'n':'o':'d':'e':xs)=
          [(WebNode . unsafePerformIO $ emptyPool, xs)]
     readsPrec _ s=
          let r= readsPrec 0 s
          in case r of
            [] -> []
            [((h,p,ss),s')] ->  [(Node h p empty ss,s')]
          where
          empty= unsafePerformIO  emptyPool

newtype MyNode= MyNode Node deriving(Read,Show,Typeable)


--instance Indexable MyNode where key (MyNode Node{nodePort=port}) =  "MyNode "++ show port
--
--instance Serializable MyNode where
--    serialize= BS.pack . show
--    deserialize= read . BS.unpack

nodeList :: TVar  [Node]
nodeList = unsafePerformIO $ newTVarIO []

deriving instance Ord PortID

--myNode :: Int -> DBRef  MyNode
--myNode= getDBRef $ key $ MyNode undefined






errorMyNode f= error $ f ++ ": Node not set. Use setMynode before listen"

#ifdef ghcjs_HOST_OS
getMyNode :: Cloud ()
getMyNode= return ()

setMyNode _ = return ()
#else

getMyNode :: Cloud Node
getMyNode = local $ do
    Connection{myNode=rnode} <- getSData <|> errorMyNode "getMyNode"
    MyNode node <- liftIO $ atomically $ readTVar rnode  -- `onNothing` errorMyNode "getMyNode"
    return node



setMyNode :: Node -> TransIO ()
setMyNode node= Transient $ do
        addNodes [node]
        Just events <- runTrans newEVar
        rnode <- liftIO $ newTVarIO $ MyNode node
        let conn= Connection rnode Nothing 8192 events (unsafePerformIO $ newMVar ()) False 0  :: Connection
        setData conn
        return $ Just ()
#endif

-- | return the list of nodes connected to the local node
getNodes :: MonadIO m => m [Node]
getNodes  = liftIO $ atomically $ readTVar  nodeList

addNodes :: (MonadIO m, MonadState EventF m) => [Node] -> m ()
addNodes   nodes=  do
#ifndef ghcjs_HOST_OS
  mapM_ verifyNode nodes -- if the node is a web one, add his connection
#endif
  liftIO . atomically $ do
    prevnodes <- readTVar nodeList
    writeTVar nodeList $ nub $ nodes ++ prevnodes

#ifndef ghcjs_HOST_OS
verifyNode (WebNode pool)= do
  r <- getData `onNothing` error "adding web node without connection set"
  case r of
   conn@(Connection{connData= Just( Node2Web ws)}) ->
            liftIO $ writeIORef pool [conn]
   other -> return ()

verifyNode n= return ()
#endif

shuffleNodes :: MonadIO m => m [Node]
shuffleNodes=  liftIO . atomically $ do
  nodes <- readTVar nodeList
  let nodes'= tail nodes ++ [head nodes]
  writeTVar nodeList nodes'
  return nodes'

--getInterfaces :: TransIO TransIO HostName
--getInterfaces= do
--   host <- logged $ do
--      ifs <- liftIO $ getNetworkInterfaces
--      liftIO $ mapM_ (\(i,n) ->putStrLn $ show i ++ "\t"++  show (ipv4 n) ++ "\t"++name n)$ zip [0..] ifs
--      liftIO $ putStrLn "Select one: "
--      ind <-  input ( < length ifs)
--      return $ show . ipv4 $ ifs !! ind


-- | execute a Transient action in each of the nodes connected.
--
-- The response of each node is received by the invoking node and processed by the rest of the procedure.
-- By default, each response is processed in a new thread. To restrict the number of threads
-- use the thread control primitives.
--
-- this snippet receive a message from each of the simulated nodes:
--
-- > main = keep $ do
-- >    let nodes= map createLocalNode [2000..2005]
-- >    addNodes nodes
-- >    (foldl (<|>) empty $ map listen nodes) <|> return ()
-- >
-- >    r <- clustered $ do
-- >               Connection (Just(PortNumber port, _, _, _)) _ <- getSData
-- >               return $ "hi from " ++ show port++ "\n"
-- >    liftIO $ putStrLn r
-- >    where
-- >    createLocalNode n= createNode "localhost" (PortNumber n)
clustered :: Loggable a  => Cloud a -> Cloud a
clustered proc= loggedc $ do
     nodes <-  onAll getNodes
     foldr (<|>) empty $ map (\node -> callTo node proc) nodes  -- !!> "fold"



-- A variant of `clustered` that wait for all the responses and `mappend` them
mclustered :: (Monoid a, Loggable a)  => Cloud a -> Cloud a
mclustered proc= loggedc $ do
     nodes <-  onAll getNodes
     foldl (<>) mempty $ map (\node -> runAt node proc) nodes  -- !!> "fold"

-- | Initiates the transient monad, initialize it as a new node (first parameter) and connect it
-- to an existing node (second parameter).
-- The other node will notify about this connection to
-- all the nodes connected to him. this new connected node will receive the list of nodes
-- the local list of nodes then is updated with this list. it can be retrieved with `getNodes`

connect ::  Node ->  Node -> Cloud ()
connect  node  remotenode =   do
    listen node <|> return () -- listen1 node remotenode
    local $ liftIO $ putStrLn $ "connecting to: "++ show remotenode
    newnode <- local $ return node -- must pass my node to the remote node or else it will use his own

    nodes <- runAt remotenode $  do
                   mclustered $ onAll $ addNodes [newnode]
                   onAll $ do
                      liftIO $ putStrLn $ "Connected node: " ++ show node
                      getNodes

    onAll $ liftIO $ putStrLn $ "Connected to nodes: " ++ show nodes
    onAll $ addNodes nodes

{-
#ifndef ghcjs_HOST_OS
listen1 node remotenode = listen node <|> return ()
#else
listen1 node remotenode = onAll $ do
       conn <- mconnect remotenode
       release remotenode conn
       setSData conn                    -- !!>  "OPENED in listen1"

       do
           r <- mread conn
           log <- case r of
                     SError e -> do

                           release remotenode conn
                           error $ show e
                     SDone ->  do
                          release remotenode conn
                          empty

                     SMore log -> return log
                     SLast log -> do
                         release remotenode conn
                         return log

           setSData $ Log True log (reverse log)   -- !!> show log

        <|> return ()



#endif
-}

--------------------------------------------


#ifndef ghcjs_HOST_OS
httpMode (method,uri, headers) conn  = do
   if isWebSocketsReq headers
     then  liftIO $ do

         stream <- makeStream                  -- !!> "WEBSOCKETS request"
            (do
                bs <- SBS.recv conn  4096
                return $ if BC.null bs then Nothing else Just bs)
            (\mbBl -> case mbBl of
                Nothing -> return ()
                Just bl ->  SBS.sendMany conn (BL.toChunks bl) >> return())   -- !!> show ("SOCK RESP",bl)

         let
             pc = WS.PendingConnection
                { WS.pendingOptions     = WS.defaultConnectionOptions
                , WS.pendingRequest     =  NS.RequestHead uri headers False -- RequestHead (BC.pack $ show uri)
                                                      -- (map parseh headers) False
                , WS.pendingOnAccept    = \_ -> return ()
                , WS.pendingStream      = stream
                }


         sconn    <- WS.acceptRequest pc               -- !!> "accept request"
         WS.forkPingThread sconn 30
         return sconn



     else do
          let uri'= BC.tail $ uriPath uri              -- !!> "HTTP REQUEST"
              file= if BC.null uri' then "index.html" else uri'

          content <- liftIO $  BL.readFile ( "tests/Test.jsexe/"++ BC.unpack file)
                            `catch` (\(e:: SomeException) -> return "NOT FOUND")

          n <- liftIO $ SBS.sendMany conn $ -- ["HTTP/1.0 200 OK\rContent-Type: text/html\r\r"] ++
                                  (BL.toChunks content )

          empty

      where
      uriPath = BC.dropWhile (/= '/')



isWebSocketsReq = not  . null
    . filter ( (== mk "Sec-WebSocket-Key") . fst)



data ParseContext a = ParseContext (IO  a) a deriving Typeable


--giveData h= do
--    r <- BC.hGetLine h
--    return r !> ( "RECEIVED "++ show r)

giveData h= do

   r <- readIORef rend

   if r then return "" else do
    r<- BC.hGetLine h                    -- !!> "GETLINE"

    if r=="\r" || r == "" then do
       writeIORef rend True
       return ""
       else return r
  where
  rend= unsafePerformIO $ newIORef False


receiveHTTPHead h = do
  setSData $ ParseContext (giveData h) ""
  (method, uri, vers) <- (,,) <$> getMethod <*> getUri <*> getVers
  headers <- many $ (,) <$> (mk <$> getParam) <*> getParamValue    --  !!> show (method, uri, vers)
  return (method, uri, headers)                                    --  !!> show (method, uri, headers)


getMethod= getString
getUri= getString
getVers= getString
getParam= do
      dropSpaces

      r <- tTakeWhile (\x -> x /= ':' && x /= '\r')
      if BC.null r || r=="\r"  then  empty  else  dropChar >> return r

getParamValue= dropSpaces >> tTakeWhile  (/= '\r')

dropSpaces= parse $ \str ->((),BC.dropWhile isSpace str)

dropChar= parse  $ \r -> ((), BC.tail r)

getString= do
    dropSpaces

    tTakeWhile (not . isSpace)

tTakeWhile :: (Char -> Bool) -> TransIO BC.ByteString
tTakeWhile cond= parse (BC.span cond)

parse :: (Typeable a, Eq a, Show a, Monoid a,Monoid b) => (a -> (b,a)) -> TransIO b
parse split= do
    ParseContext rh str <- getSData <|> error "parse: ParseContext not found"
    if  str == mempty then do
          str3 <- liftIO  rh

          setSData $ ParseContext rh str3

          if str3== mempty then empty   else  parse split

       else do

          cont <- do
             let (ret,str3) = split str
             setSData $ ParseContext rh str3
             if  str3 == mempty
                then  return $ Left ret
                else  return $ Right ret
          case cont of
            Left r  ->  (<>) <$> return r  <*> (parse split <|> return mempty)

            Right r ->   return r




#endif

#ifdef ghcjs_HOST_OS
isBrowserInstance= True

listen _ = return () -- error "listen not implemented in browser"
#else
isBrowserInstance= False
#endif
