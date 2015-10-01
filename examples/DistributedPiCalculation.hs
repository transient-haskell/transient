{-# LANGUAGE MonadComprehensions #-}
import Transient.Base
import Transient.Move
import Transient.Indeterminism
import Network
import Data.IORef
import Control.Applicative
import System.Random
import Control.Monad.IO.Class
import GHC.Conc
import System.IO
import Control.Monad

mainonce= do
   let numNodes= 5
       numSamples= 1000
       ports= [2000.. 2000+ numNodes -1]
       createLocalNode p= createNode "localhost"  p
       nodes= map createLocalNode ports

   addNodes nodes
   keep $  threads 1 $ do

     foldl (<|>) empty (map listen nodes) <|> return()

     xs <- collect numSamples $ clustered [if x * x + y * y < 1 then 1 else (0 :: Int)| x <- random, y <-random]
     liftIO $ print (4.0 * (fromIntegral $ sum xs) / (fromIntegral numSamples) :: Double)

     where
     random= waitEvents $ liftIO  randomIO :: TransIO Double


main= do
   let numNodes= 5
       numCalcsNode= 100
       ports= [2000.. 2000+ numNodes -1]
       createLocalNode p= createNode "localhost"  p
       nodes= map createLocalNode ports

   rresults <- newIORef (0,0)
   keep $ freeThreads $ threads 1 $ do
     addNodes nodes
     foldl (<|>) empty (map listen nodes) <|> return()

     r <- clustered $ do
               Connection (Just (_,h,_,_)) _ <- getSData <|> error "no connection"
               liftIO $ hSetBuffering h $ BlockBuffering Nothing
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
