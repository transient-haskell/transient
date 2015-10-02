-- Distributed streaming using Transient
-- See the article: https://www.fpcomplete.com/tutorial-edit/streaming-transient-effects-vi

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, MonadComprehensions  #-}
module Main where
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


main= mainOnce

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

mainOnce= do
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

     where
     random=  waitEvents' $ liftIO  randomIO :: TransIO Double


-- continuos streaming version
-- Perform the same calculation but it does not stop, and the results are accumulated in in a mutable reference within the calling node,
-- so the precision in the value of pi is printed with more and more precision. every 1000 calculations.

-- Here instead of `collect` that finish the calculation when the number of samples has been reached, i use `group` which simply
-- group the number of results in a list and then sum of the list is returned to the calling node.

-- Since `group` do not finish the calculation, new sums are streamed from the nodes again and again.

mainContinuous= do
   let numNodes= 5
       numCalcsNode= 100
       ports= [2000.. 2000+ numNodes -1]
       createLocalNode p= createNode "localhost"  p
       nodes= map createLocalNode ports

   rresults <- newIORef (0,0)
   keep $ freeThreads $ threads 1 $ do
     --setBufSize 1024
     addNodes nodes
     foldl (<|>) empty (map listen nodes) <|> return()

     r <- clustered $ do
               --Connection (Just (_,h,_,_)) _ <- getSData <|> error "no connection"
               --liftIO $ hSetBuffering h $ BlockBuffering Nothing
               r <- group numCalcsNode $ do
                         n <- liftIO  getNumCapabilities
                         threads n .
                          spawn $ do
                           x <- randomIO :: IO Double
                           y <- randomIO
                           return $ if x * x + y * y < 1 then 1 else (0 :: Int)
               return $ sum r

     (n,c) <- liftIO $ atomicModifyIORef' rresults $ \(num, count) ->
                let num' = num + r
                    count'= count + numCalcsNode
                in ((num', count'),(num',count'))

     when ( c `rem` 1000 ==0) $ liftIO $ do
           th <- myThreadId
           putStrLn $ "Samples: "++ show c ++ " -> " ++
             show( 4.0 * fromIntegral n / fromIntegral c)++ "\t" ++ show th

-- really distributed version
-- generate an executable with this main and invoke it as such:
--
-- program  myport  remotehost  remoteport
--
-- where remotehost remoteport are from a previously initialized node
-- The first node initialize it with:
--
-- program myport  localhost myport

mainDistributed= do
    args <- getArgs
    let localPort = read (args !! 0)
        seedHost  = read (args !! 1)
        seedPort  = read (args !! 2)

        mynode    = createNode "localhost"  localPort
        seedNode  = createNode seedHost seedPort
        numCalcsNode= 100
    rresults <- liftIO $ newIORef (0,0)

    keep $ do
       connect mynode seedNode

       logged $ option  "start"  "start the calculation once all the nodes have been started"  :: TransIO String


       r <- clustered $ do
               --Connection (Just (_,h,_,_)) _ <- getSData <|> error "no connection"
               --liftIO $ hSetBuffering h $ BlockBuffering Nothing
               r <- group numCalcsNode $ do
                         n <- liftIO  getNumCapabilities
                         threads n .
                          spawn $ do
                           x <- randomIO :: IO Double
                           y <- randomIO
                           return $ if x * x + y * y < 1 then 1 else (0 :: Int)
               return $ sum r

       (n,c) <- liftIO $ atomicModifyIORef' rresults $ \(num, count) ->
                let num' = num + r
                    count'= count + numCalcsNode
                in ((num', count'),(num',count'))

       when ( c `rem` 1000 ==0) $ liftIO $ do
           th <- myThreadId
           putStrLn $ "Samples: "++ show c ++ " -> " ++
             show( 4.0 * fromIntegral n / fromIntegral c)++ "\t" ++ show th



mainStreamFiles= keep . threads 0  $ do
         chunk <- sourceFile "../src/Main.hs"
         liftIO $ print chunk
         return $ map toUpper chunk
       `sinkFile` "outfile"

