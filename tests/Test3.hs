module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Environment
import           System.IO
import           Transient.Base
import           Transient.Indeterminism
import           Transient.Logged
import           Transient.Move
import           Transient.Stream.Resource
import           Control.Applicative
import           System.Info
import           Control.Concurrent

main = do

  let nodes= [createNode "localhost" 2020, createNode "192.168.99.100" 2020]
  args  <- getArgs
  let [localnode, remote]= if length args > 0 then nodes
                                     else reverse nodes


  runCloud' $ do
    onAll $ addNodes nodes
    listen localnode <|> return  ()
    hello <|> helloworld <|> stream localnode

hello= do
    local $  option  "hello"  "each computer say hello"

    r <- clustered  $  do
                      node <- getMyNode
                      onAll . liftIO . print $ "hello " ++ os
                      return ("hello from",os,arch, nodeHost node)

    lliftIO $ print r

helloworld= do
    local $ option "helloword"  "both computers compose \"hello world\""
    r <- mclustered  $  return $ if  os== "linux" then "hello " else "world"
    lliftIO $ print r


stream remoteHost= do
    local $ option "stream" "stream from the Linux node to windows"
    let fibs=  0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]   -- fibonacci numbers

    r <- runAt remoteHost $ local $ do
                     r <-  threads 1 $ choose $ take 10 fibs
                     liftIO $ putStr os >> print r
                     liftIO $ threadDelay 1000000
                     return r
    lliftIO $ print r
