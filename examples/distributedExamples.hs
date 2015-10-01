{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, MonadComprehensions #-}
module Main where

import Transient.Move
import Transient.Logged
import Transient.Base
import Transient.Indeterminism
import Transient.EVars
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
import Data.IORef

-- some tests for distributed computing

--main= do
--      let port1 = PortNumber 2000
--          port2 = PortNumber 2001
--
--
--      keep $  do
--        conn port1 port1 <|> conn port2 port1
--
--        examples' host port2
--      where
--      host= "localhost"
----      delay = liftIO $ threadDelay 1000000
--      conn p p'=  connect host p host p'
--



main = do
   args <- getArgs
   let ports= [ 2000,  2001]

   let [port1, port2]= if null args  then ports else reverse ports
       local= "localhost"
   print [port1, port2]
   let mynode= createNode local port1
   let node = createNode local port2
   addNodes [mynode,node]
   beamInit  mynode $ do
        logged $ option "call" "call"


 --       (logged $ async $ return (1:: Int))  <|> (logged $ async $ return 2)
        rs <- streamFrom node $ do
                   liftIO $ print "hello"
                   x <-  choose[1..5] :: TransIO Int
                   liftIO $ putStrLn $ "remote creating chunk of" ++ show x
                   return  . SMore $ replicate 10 x


        liftIO $ putStrLn $ "packet arrived of "++ show rs

--       r <-   callTo node [(x,y)| x <- choose[1..4], y <- choose[1..(4 ::Int)] , x-y == 1]
--
--       liftIO $ print r

--      roundtrip 5 node   <|> roundtrip 5 node
--      where
--      roundtrip 0 _ = return ()
--      roundtrip n (node@(Node _ port2 _))=  do
--           beamTo node
--           step $ liftIO $ print "PING"
--
--           beamTo node
--           step $ liftIO $ print "PONG"
--           roundtrip (n - 1) node


-- to be executed with two or more nodes
two = do
  args <- getArgs
  if length args < 2
    then do
       putStrLn "The program need at least two parameters:  localHost localPort  remoteHost RemotePort"
       putStrLn "Start one node alone. The rest, connected to this node."
       return ()
    else keep $ do
       let localHost= head args
           localPort=  read $ args !! 1
           (remoteHost,remotePort) =
             if length args >=4
                then(args !! 2, read $ args !! 3)
                else (localHost,localPort)
       let localNode=  createNode localHost localPort
           remoteNode= createNode remoteHost remotePort
       connect localNode remoteNode
       examples


examples =  do
   logged $ option "main"  "to see the menu" <|> return ""
   r <- logged   $ option "move" "move to another node"
               <|> option "call" "call a function in another node"
               <|> option "chat" "chat"
               <|> option "netev" "events propagating trough the network"
   case r of
       "call"  -> callExample
       "move"  -> moveExample
       "chat"  -> chat
       "netev" -> networkEvents

data Environ= Environ (IORef String) deriving Typeable

callExample = do
   nodes <- logged getNodes
   let node=  head $ tail nodes -- the first connected node

   logged $ putStrLnhp  node "asking for the remote data"
   s <- callTo node $  do
                       putStrLnhp  node "remote callTo request"
                       liftIO $ readIORef environ


   liftIO $ putStrLn $ "resp=" ++ show s

{-# NOINLINE environ #-}
environ= unsafePerformIO $ newIORef "Not Changed"

moveExample = do
   nodes <- logged getNodes
   let node=  head $ tail nodes

   putStrLnhp  node "enter a string. It will be inserted in the other node by a migrating program"
   name <- logged $ input (const True)
   beamTo node
   putStrLnhp  node "moved!"
   putStrLnhp  node $ "inserting "++ name ++" as new data in this node"


   liftIO $ writeIORef environ name
   return()


chat ::  TransIO ()
chat  = do
    name  <- logged $ do liftIO $ putStrLn "Name?" ; input (const True)
    text <- logged $  waitEvents  $ putStr ">" >> hFlush stdout >> getLine' (const True)
    let line= name ++": "++ text
    clustered $   liftIO $ putStrLn line


networkEvents = do
     nodes <- logged getNodes
     let node=  head $ tail nodes
     logged $ putStrLnhp  node "write \"fire\" in the other node"

     r <- callTo node $ do
                         option "fire"  "fire event"
                         return "event fired"
     putStrLnhp  node $ r ++ " in remote node"

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

