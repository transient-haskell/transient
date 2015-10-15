-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move.Services
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

module Transient.Move.Services  where

import Transient.Base
import Transient.Move
import Transient.Logged
import Transient.EVars
import Control.Monad.IO.Class
import System.Process
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Applicative
import Network (PortID(..))
import GHC.Conc
import System.Directory
import Control.Monad
import Data.List
import Data.TCache hiding(onNothing)

-- for the example
import System.Environment

startServices :: TransIO ()
startServices= do
  node <- getMyNode
  liftIO $ print node
  mapM_ start $ services node
  where
  start (package,program,port)= liftIO $ do
          let prog= pathExe (name package) program port
          liftIO $ print prog
          createProcess $ shell prog


pathExe package program port= package++"/dist/build/"++package++"/"++program
                                       ++ " " ++ show port

install :: String  -> String -> Int -> TransIO ()
install package program port = logged $ do
     let packagename = name package
     exist <-  logged $ liftIO $ doesDirectoryExist  packagename
     when (not exist) $ logged $ liftIO $ do
         callProcess  "git" ["clone",package]
         liftIO $ print "GIT DONE"
         setCurrentDirectory packagename
         callProcess  "cabal" ["install"]
         setCurrentDirectory ".."
         return()
     let prog= pathExe packagename program port
     logged $ liftIO $ do
           createProcess $ shell program
           return ()

     let service= (package, program,  port)

     Connection{myNode= rnode} <- getSData <|> error "Mynode not set: use setMyNode"
     logged $ liftIO $ do
       atomically $ do
        MyNode( Node h p c servs) <- readDBRef rnode
                  `onNothing` error "install: myNode: not set with setMyNode"
        writeDBRef rnode $ MyNode $ Node h p c $ service:servs
       liftIO syncCache
     node <- logged getMyNode
     clustered $ notifyService node service
     return()

name url= do
     let git= "http://github.com/"
     if not $ isPrefixOf git url
       then error "install: only github repos are admitted, sorry"
       else
        let segments = split '/' $ drop (length git) url
            segs'= reverse segments
        in  head  segs'


     where
     split c []= []
     split c xs=
        let (h,t)= span  (/= c) xs
        in  if null t then [h] else h : split c  (tail t)

rfreePort :: MVar Int
rfreePort = unsafePerformIO $ newMVar  3000

freePort :: MonadIO m => m Int
freePort= liftIO $ modifyMVar rfreePort $ \ n -> return (n+1,n)

initService node package program= logged $
    case   find  (\(package', program',_) -> package==package' && program== program') $ services node of
       Just (_,_,port) -> return port
       Nothing -> do
            beamTo node
            port <- logged freePort
            install package program  port
            stop
          <|> do
            Connection _ _ _ ev<- getSData
            (node', (package', program',port)) <- readEVar ev
            if node'== node && package' == package && program'== program
                 then return port
                 else stop

notifyService :: Node -> Service -> TransIO ()
notifyService node service=  logged $ do
     liftIO $ atomically $ do
        nodes <- readTVar nodeList
        let ([nod], nodes')= span (== node) nodes
        let nod' = nod{services=service:services nod}
        writeTVar nodeList $ nod' : nodes'
        return ()

     Connection _ _ _ ev<- getSData
     writeEVar ev (node,service)
     return ()


main= do
--      keep $ install "http://github.com/agocorona/transient" "MainStreamFiles"  3000
    let node1= createNode "localhost" 2000
    let node2= createNode "localhost" 2001
    args <-getArgs
    let [localNode,remoteNode]= if null args then [node1,node2] else [node2,node1]

    addNodes [localNode, remoteNode]
    keep $ do
      setMyNode localNode
      listen localNode <|> return ()
      step $ option "start" "start"

      logged startServices
      port <-initService remoteNode "http://github.com/agocorona/transient" "MainStreamFiles"
      liftIO $ putStrLn $ "installed at" ++ show port
--      nodes <- getNodes
--      liftIO $ print nodes
--      liftIO syncCache
--      option "end" "end"
--      liftIO $ print "END"





