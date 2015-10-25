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
  let (local, remote)= if length args > 0 then (createNode "localhost" 2000, createNode "localhost" 2001)
                                     else (createNode "localhost" 2001, createNode "localhost" 2000)
  addNodes [local, remote]
  let    numCalcsNode = 500

  rresults <- liftIO $ newIORef (0,0)

  keep $ do

    listen local <|> return  ()
    logged $  option  "start"  "Start the calculation"
    nodes <- logged getNodes
    logged $ liftIO $ putStrLn $ "receiving from nodes: " ++ show nodes

    r <- clustered  $ threads 0 $ do
                      node <- getMyNode
                      async $ return node

    liftIO $ print r

node addr = let (h:p:_) = splitOn ':' addr in createNode h (read p)

splitOn delimiter = foldr f [[]]
  where f c l@(x:xs)
          | c == delimiter = []:l
          | otherwise = (c:x):xs
