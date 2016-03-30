module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           GHC.Conc
import           Control.Applicative
import           Data.Monoid
import           Transient.Base
import           Transient.Indeterminism
import           Transient.Logged
import           Transient.Move
import           Transient.Stream.Resource
import           Transient.DDS
import Control.Concurrent
import System.IO.Unsafe
import Data.List
import Control.Exception.Base
import qualified Data.Map as M
import System.Exit

main= do
     let numNodes = 2
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports
         node1= head nodes
         node2= nodes !! 1

     r <-runCloud'' $ do
          local $ addNodes nodes
          runNodes nodes
          local $ do
              r <- collect 5 $ (+) <$> choose [1..5] <*> choose[1..5::Int]

              liftIO $ print r

          r <-  (runAt node1 (effect "node1" >> return "hello "))
                    <>  (runAt node2 (effect "node2" >> return "world" ))
          lliftIO $ print r

          effs <- getEffects

          assert (sort effs == sort [(node1,"node1"),(node2,"node2")]) $ return ()
          delEffects

          -- collect
          -- clustered
          -- mclustered
          -- <>
          --
          r <- reduce  (+)  . cmap (\w -> (w, 1 :: Int))  $ getText  words "hello world hello hi"
          assert (sort (M.toList r) == sort [("hello",2),("hi",1),("world",1)]) $ return ()


          lliftIO $ print r
          local $ exit ()


     exitSuccess

getEffects :: Loggable a =>  Cloud [(Node, a)]
getEffects=lliftIO $ readMVar effects

runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()


delEffects= lliftIO $ modifyMVar_ effects $ const $ return[]
effects= unsafePerformIO $ newMVar []

effect x= do
   node <- getMyNode
   lliftIO $ modifyMVar_ effects $ \ xs ->  return $ (node,x): xs
   return()


