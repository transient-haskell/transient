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
import Data.List((\\))






-- to be executed with two or more nodes
main = do
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
   node <- logged $ do
         nodes <-  getNodes
         myNode <- getMyNode
         return . head $ nodes \\ [myNode]

   logged $ putStrLnhp  node "asking for the remote data"
   s <- callTo node $  do
                       putStrLnhp  node "remote callTo request"
                       liftIO $ readIORef environ


   liftIO $ putStrLn $ "resp=" ++ show s

{-# NOINLINE environ #-}
environ= unsafePerformIO $ newIORef "Not Changed"

moveExample = do
   node <- logged $ do
         nodes <-  getNodes
         myNode <- getMyNode
         return . head $ nodes \\ [myNode]
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
     node <- logged $ do
         nodes  <- getNodes
         myNode <- getMyNode
         return . head $ nodes \\ [myNode]

     logged $ putStrLnhp  node "<- write \"fire\" in this other node"

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

