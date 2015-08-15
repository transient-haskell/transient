{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Transient.Move
import Transient.Logged
import Transient.Base
import Network
import Control.Applicative
import Data.IORef
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Data.Monoid
import System.IO
import Control.Monad
import Data.Maybe
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Typeable
import Control.Concurrent.STM

x |> y =  (x >> empty) <|> y

--one= do
--  let port1 = (PortNumber 2000)
--      port2 = (PortNumber 2001)
--  keep $ do
--    initialInstance port1 port2
--    addedInstance  port1 port2
--  where
--    initialInstance port1 port2= do
--         connect  "localhost" port1 "localhost" port1
--         examples
--       |> close
--
--    addedInstance port1 port2=do
--         connect "localhost" port2 "localhost"  port1
--         examples
--       |> close
--
--    close= do
--       (h,sock) <- getSData
--       liftIO $ hClose h  `catch` (\(e::SomeException) -> sClose sock)

--
--
--localPort=  (PortNumber 2000)
--localHost= "localhost"
--
--
--
--
--
--
--recursive=   do
--   beamInit localPort $ do
----         step $ addNodes localPort [(localHost,localPort)]
--         r <- callTo localHost localPort $  return [True]
--         liftIO $ print r

test= do
   args <- getArgs
   let ports= [("localhost",PortNumber 2000), ("localhost",PortNumber 2001)]

   let [(_,port1), (_,port2)]= if null args  then ports else reverse ports
   print [port1, port2]

   beamInit port1 $ do
--         step $ addNodes port1 [head ports]-- ports
         step $ liftIO getChar -- option "call" "call"
--         r <- callTo "localhost" port1  $ callTo "localhost" port2 $ liftIO $ print "hello"
         r <-  (callTo "localhost" port2 (liftIO $ print "HELLO" >> return "1") >>= step . return . ("x" ++))
              <> callTo "localhost" port2 (liftIO $ print "HOLA"  >> return "2")
              <> callTo "localhost" port2 (liftIO $ print "CIAO"  >> return "3")
         liftIO $ print r

main = do
  args <- getArgs
  if length args < 2
    then do
     putStrLn "The program need at least two parameters:  localHost localPort  remoteHost RemotePort"
     putStrLn "Start one node alone. The rest, connected to this node."
     return ()
    else keep $ do
       let localHost= head args
           localPort= PortNumber . fromInteger . read $ args !! 1
           (remoteHost,remotePort) =
             if length args >=4
                then(args !! 2, PortNumber . fromInteger . read $ args !! 3)
                else (localHost,localPort)
       connect localHost localPort remoteHost remotePort
       examples localPort


examples localPort= do
   nodes <- step getAllNodes
   step $ liftIO $ print $ "NODES=" ++ show  nodes
   let (remoteHost,remotePort)=  head  nodes
   r <-step $ oneThread
                 $ option "move" "move to another node"
               <|> option "call" "call a function in another node"
               <|> option "chat" "chat"
   case r of
       "call" -> callExample remoteHost remotePort
       "move" -> moveExample remoteHost remotePort
       "chat" -> chat localPort


callExample host port= do
   step $ liftIO $ putStrLn "asking for the remote data"
   s <- callTo host port $ liftIO $ do
                       putStrLn "remote callTo request"
                       readIORef environ


   liftIO $ putStrLn $ "resp=" ++ show s


environ= unsafePerformIO $ newIORef "Not Changed"

moveExample host port= do
   step $ liftIO $ putStrLn "enter a string. It will be inserted in the other node by a migrating program"
   name <- step $ input (const True)
   beamTo host port
   liftIO $ print "moved!"
   liftIO $ print $ "inserting "++ name ++" as new data in this node"
   liftIO $ writeIORef environ name
   return()


chat :: PortID -> TransIO ()
chat port' = do
    name  <- step $ do liftIO $ putStrLn "Name?" ; input (const True)
    text <- step $  waitEvents  $ putStr ">" >> hFlush stdout >> getLine' (const True)
    let line= name ++": "++ text
    port <- step $ return port'

    nodes <- getAllNodes
    clustered $ liftIO $ putStrLn line
--    callTo h p (liftIO $ putStrLn line)  <|>  callTo h' p' (liftIO $ putStrLn line)
--    r <- foldl (<>) mempty $ map (\(h,n) -> callTo h  n  $ liftIO $  putStrLn line >> return [True]) nodes
--    liftIO $ print r
--    liftIO $ print r

