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
import Control.Monad.State
import Unsafe.Coerce
import qualified Data.Map as M

main= do
     let numNodes = 2
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports
         node1= head nodes
         node2= nodes !! 1

     runCloud'   $ do
--          local $ addNodes nodes
--          runNodes nodes

          local $   (sync $ async( threadDelay 1000000 >> print "hello") >> stop ) <|> (liftIO $print "world")
--         (liftIO (print "world") >>stop) <|> (liftIO $ print "hello")




--     print r


sync :: TransIO a -> TransIO a
sync x=  Transient $ do

        EventF _ _ x' fs _ _ _ _ _ _ _  <- get



--        setContinuation x (\x -> liftIO (print "hi") >> return x)  $   fs
        r <- runTrans $ unsafeCoerce x'

--        setData WasRemote
--        restoreStack fs
        return r

getEffects :: Loggable a =>  Cloud [(Node, a)]
getEffects=lliftIO $ readMVar effects

runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()


delEffects= lliftIO $ modifyMVar_ effects $ const $ return[]
effects= unsafePerformIO $ newMVar []

effect x= do
   node <- getMyNode
   lliftIO $ modifyMVar_ effects $ \ xs ->  return $ (node,x): xs
   return()


