module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           GHC.Conc
import           System.Environment
import           System.IO
import           System.Random
import           Transient.Base
import           Transient.Indeterminism
import           Transient.Logged
import           Transient.Move
import           Transient.Stream.Resource
import           Control.Applicative


main = do
  args  <- getArgs
  let (self : master : _) = node <$> args
      numCalcsNode = 5000

  rresults <- liftIO $ newIORef (0,0)

  keep $ do
    connect self master
    logged $ do
        option  "start"  "Start the calculation"
        nodes <- getNodes
        liftIO $ putStrLn $ "receiving from nodes: " ++ show nodes


    r <- clustered $ do
          r <- group numCalcsNode $ do
            n <- liftIO  getNumCapabilities
            threads n $ spawn $ do
              x <- randomIO :: IO Double
              y <- randomIO
              return $ if x * x + y * y < 1 then 1 else (0 :: Int)
          return $ sum r

    (n,c) <- liftIO $ atomicModifyIORef' rresults $ \(num, count) ->
              let num' = num + r
                  count'= count + numCalcsNode
              in ((num', count'),(num',count'))

    when ( c `rem` 100000 == 0) $ liftIO $ do
      th <- myThreadId
      putStrLn $ "Samples: " ++ show c ++ " -> " ++
        show( 4.0 * fromIntegral n / fromIntegral c) ++ "\t" ++ show th


node addr = let (h:p:_) = splitOn ':' addr in createNode h (read p)

splitOn delimiter = foldr f [[]]
  where f c l@(x:xs)
          | c == delimiter = []:l
          | otherwise = (c:x):xs
