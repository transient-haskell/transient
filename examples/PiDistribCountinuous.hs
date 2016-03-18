-- Distributed streaming using Transient
-- See the article: https://www.fpcomplete.com/user/agocorona/streaming-transient-effects-vi

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, MonadComprehensions  #-}

module MainCountinuous where
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



-- continuos streaming version
-- Perform the same calculation but it does not stop, and the results are accumulated in in a mutable reference within the calling node,
-- so the precision in the value of pi is printed with more and more precision. every 1000 calculations.

-- Here instead of `collect` that finish the calculation when the number of samples has been reached, i use `group` which simply
-- group the number of results in a list and then sum of the list is returned to the calling node.

-- Since `group` do not finish the calculation, new sums are streamed from the nodes again and again.

main= do
   let numNodes= 5
       numCalcsNode= 100
       ports= [2000.. 2000+ numNodes -1]
       createLocalNode p= createNode "localhost"  p
       nodes= map createLocalNode ports

   rresults <- newIORef (0,0)
   keep $ freeThreads $ threads 10 $ runCloud $ do
     --setBufSize 1024
     local $ addNodes nodes
     foldl (<|>) empty (map listen nodes) <|> return()

     r <- clustered $ do
               --Connection (Just (_,h,_,_)) _ <- getSData <|> error "no connection"
               --liftIO $ hSetBuffering h $ BlockBuffering Nothing
               r <- local $ group numCalcsNode $ do
                         n <- liftIO  getNumCapabilities
                         threads n .
                          spawn $ do
                           x <- randomIO :: IO Double
                           y <- randomIO
                           return $ if x * x + y * y < 1 then 1 else (0 :: Int)
               return $ sum r

     (n,c) <- local $ liftIO $ atomicModifyIORef' rresults $ \(num, count) ->
                let num' = num + r
                    count'= count + numCalcsNode
                in ((num', count'),(num',count'))

     when ( c `rem` 1000 ==0) $ local $  liftIO $ do
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

    runCloud' $ do
       connect mynode seedNode

       local $ option  "start"  "start the calculation once all the nodes have been started"  :: Cloud String

       r <- clustered $ do
               --Connection (Just (_,h,_,_)) _ <- getSData <|> error "no connection"
               --liftIO $ hSetBuffering h $ BlockBuffering Nothing
               r <- local $ group numCalcsNode $ do
                         n <- liftIO  getNumCapabilities
                         threads n .
                          spawn $ do
                           x <- randomIO :: IO Double
                           y <- randomIO
                           return $ if x * x + y * y < 1 then 1 else (0 :: Int)
               return $ sum r

       (n,c) <- local $ liftIO $ atomicModifyIORef' rresults $ \(num, count) ->
                let num' = num + r
                    count'= count + numCalcsNode
                in ((num', count'),(num',count'))

       when ( c `rem` 1000 ==0) $ local $ liftIO $ do
           th <- myThreadId
           putStrLn $ "Samples: "++ show c ++ " -> " ++
             show( 4.0 * fromIntegral n / fromIntegral c)++ "\t" ++ show th
