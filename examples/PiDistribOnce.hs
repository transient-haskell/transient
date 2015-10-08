-- Distributed streaming using Transient
-- See the article: https://www.fpcomplete.com/tutorial-edit/streaming-transient-effects-vi

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, MonadComprehensions  #-}
module MainOnce where
import Transient.Base
import Transient.Move
import Transient.Indeterminism
import Transient.Logged
import Transient.Stream.Resource
import Control.Applicative
import Data.Char
import Control.Monad
import Control.Monad.IO.Class
import System.Random
import Data.IORef
import System.IO
import GHC.Conc
import System.Environment



-- distributed calculation of PI
-- This example program is the closest  one to the defined in the spark examples: http://tldrify.com/bpr
-- But while the spark example does not contain the setup of the cluster and the confuguration/initalization
-- this examples includes everything

-- The nodes are simulated within the local process, but they communicate trough sockets and serialize data
-- just like real nodes. Each node spawn threads and return the result to the calling node.
-- when the number of result are reached `colect` kill the threads, the sockets are closed and the stream is stopped

-- for more details look at the article: https://www.fpcomplete.com/tutorial-edit/streaming-transient-effects-vi
--
-- there is a pending problem with the killing of the spawned threads in the remote nodes.
-- so I force the exit at the end of the calculation

main= do
   let numNodes= 5
       numSamples= 1000
       ports= [2000.. 2000 + numNodes -1]
       createLocalNode p= createNode "localhost"  p
       nodes= map createLocalNode ports

   addNodes nodes
   keep $ do
--     logged $ option "start" "start"
     xs <- collect numSamples $ do
             foldl (<|>) empty (map listen nodes) <|> return()
             clustered[if x * x + y * y < 1 then 1 else (0 :: Int)| x <- random, y <-random]

     liftIO $ print (4.0 * (fromIntegral $ sum xs) / (fromIntegral numSamples) :: Double)
     exit
     
     where
     random=  waitEvents' $ liftIO  randomIO :: TransIO Double

