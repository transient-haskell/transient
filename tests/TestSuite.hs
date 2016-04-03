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
     let numNodes = 3
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports
         node1= head nodes
         node2= nodes !! 1
         node3= nodes !! 2

     r <-runCloud'' $ do
          local $ addNodes nodes
          runNodes nodes
          local $ do
              r <- collect 5 $ (+) <$> choose [1..5]
              liftIO $ print r

              ev <- newEVar
              r <- collect 3 $ readEVar ev <|> ((choose [1..3] >> writeEVar ev) >> stop)

              assert (sort r== [1,2,3]) $ print r

          r <-  (runAt node1 (effect "node1" >> return "hello "))
                    <>  (runAt node2 (effect "node2" >> return "world" ))

          assert(r== "hello world") $ lliftIO $ print r
          effs <- getEffects
          assert (sort effs == sort [(node1,"node1"),(node2,"node2")]) $ return ()
          delEffects


          -- monadic
          r <- runAt node1 (effect "node1" >>
                   runAt node2 (effect "node2" >>
                        runAt node3 (effect "node3" >> return "hello")))

          assert(r== "hello") $ lliftIO $ print r
          effs <- getEffects
          assert (sort effs == sort [(node1,"node1"),(node2,"node2"),(node3,"node3")]) $ return ()
          delEffects


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


