{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module Main where

import Transient.Move
import Transient.Logged
import Transient.Base
import Network
import Control.Applicative

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
import Data.IORef


main= do
      let port1 = PortNumber 2000
          port2 = PortNumber 2001


      keep $  do
        conn port1 port1 <|> conn port2 port1

        examples' host port2
      where
      host= "localhost"
--      delay = liftIO $ threadDelay 1000000
      conn p p'=  connect host p host p'





test = do
   args <- getArgs
   let ports= [("localhost",PortNumber 2000), ("localhost",PortNumber 2001)]

   let [(_,port1), (_,port2)]= if null args  then ports else reverse ports
   print [port1, port2]
   let local= "localhost"
   beamInit port1 $ do
           logged $ option "call" "call"
           callTo local port2 (liftIO $ print "HOLA")
             <|> callTo local port2(liftIO $ print "HELLO")



two = do
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
       examples


examples = do
   nodes <- logged getNodes
   logged $ liftIO $ print $ "NODES=" ++ show  nodes
   let (remoteHost,remotePort)=  head $ tail nodes
   examples' remoteHost remotePort

examples' remoteHost remotePort= do
   logged $ option "main"  "to see the menu" <|> return ""
   r <-logged      $ option "move" "move to another node"
               <|> option "call" "call a function in another node"
               <|> option "chat" "chat"
               <|> option "netev" "events propagating trough the network"
   case r of
       "call" -> callExample remoteHost remotePort
       "move" -> moveExample remoteHost remotePort
       "chat" -> chat
       "netev" -> networkEvents remoteHost remotePort

data Environ= Environ (IORef String) deriving Typeable

callExample host port= do
   logged $ putStrLnhp  port "asking for the remote data"
   s <- callTo host port $  do
                       putStrLnhp  port "remote callTo request"
                       Environ environ <- getSData <|> do
                                    ref <- liftIO $ newIORef "Not Changed"
                                    let env= Environ ref
                                    setSData env
                                    return env
                       liftIO $ readIORef environ


   liftIO $ putStrLn $ "resp=" ++ show s


environ= unsafePerformIO $ newIORef "Not Changed"

moveExample host port= do
   putStrLnhp  port "enter a string. It will be inserted in the other node by a migrating program"
   name <- logged $ input (const True)
   beamTo host port
   putStrLnhp  port "moved!"
   putStrLnhp  port $ "inserting "++ name ++" as new data in this node"
   Environ environ <- getSData <|> do
                                    ref <- liftIO $ newIORef "Not Changed"
                                    let env= Environ ref
                                    setSData env
                                    return env

   liftIO $ writeIORef environ name
   return()


chat ::  TransIO ()
chat  = do
    name  <- logged $ do liftIO $ putStrLn "Name?" ; input (const True)
    text <- logged $  waitEvents  $ putStr ">" >> hFlush stdout >> getLine' (const True)
    let line= name ++": "++ text
    clustered $   liftIO $ putStrLn line


networkEvents rh rp= do
     logged $  do
       putStrLnhp  rp "callTo is not  a simole remote call. it stablish a connection"
       putStrLnhp  rp "between transient processes in different nodes"
       putStrLnhp  rp "in this example, events are piped back from a remote node to the local node"

     r <- callTo rh rp $ do
                         option "fire"  "fire event"
                         return "event fired"
     putStrLnhp  rp $ r ++ " in remote node"

putStrLnhp p msg= liftIO $ putStr (show p) >> putStr " ->" >> putStrLn msg





--call host port proc params= do
--   port <- getPort
--   listen port <|> return
--   parms <- logged $ return params
--   callTo host port proc parms
--   close
--
--distribute proc= do
--   case dataFor proc
--    Nothing -> proc
--    Just dataproc -> do
--      (h,p) <- bestNode dataproc
--      callTo h p proc
--
--bestNode dataproc=
--   nodes <- getNodes
--      (h,p) <- bestMatch dataproc nodes   <- user defined
--
--bestMatch (DataProc nodesAccesed cpuLoad resourcesNeeded)  nodes= do
--   nodesAccesed: node, response
--
--bestMove= do
--   thisproc <- gets myProc
--   case dataFor thisproc
--      Nothing -> return ()
--      Just dataproc -> do
--        (h,p) <- bestNode dataproc
--        moveTo h p
--
--
--inNetwork= do
--   p <- getPort
--   listen p <|> return ()

