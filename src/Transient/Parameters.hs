{-# LANGUAGE ExistentialQuantification , ScopedTypeVariables, FlexibleInstances #-}
module Main where

import Transient.Internals
import Transient.Move
import Transient.Move.Utils
import Data.Typeable
import Data.Map as M
import System.Random
import System.IO.Unsafe
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad

import Control.Applicative
import Transient.Indeterminism
import Control.Concurrent
import Control.Exception hiding (onException)
import System.CPUTime
-- -- opcion reactiva= parameter= EVar o mailbox

-- data Parameter=  Parameter String Int

parameters= unsafePerformIO $  newIORef  $ M.empty 

setParameter :: String  -> Int -> TransIO ()
setParameter n v= do
     vec <- liftIO $ readIORef parameters
     putMailbox' n v
     liftIO $ do
          writeIORef parameters $ M.insert  n v vec
          
addParameter :: String  -> Int -> TransIO ()
addParameter n v= liftIO $ do
     vec <- readIORef parameters
     writeIORef parameters $ M.insert  n v vec

getParameter ::  String -> Int -> TransIO Int
getParameter n v= 
     oneThread $ getMailbox' n <|> getParameterNR n v

getParameterNR ::  String -> Int -> TransIO Int
getParameterNR n v= do 
    map <- liftIO $ readIORef parameters 
    case  M.lookup n map of
      Nothing -> addParameter n v >> return v
      Just v  -> return v

optimize :: TransIO Int -> TransIO ()
optimize expr= do
     v <- expr  !> "OPTIMIZE"
     (randparam,old) <- perturbe 
     v' <- expr
     when (v > v') $ setParameter randparam old 
     optimize expr

perturbe =  do
    vec <- liftIO $ readIORef parameters !> "PERTURBE"
    i <-  liftIO $ randomRIO (0,M.size vec -1)
    let (name, pvalue) = M.toList vec !! i !> i
    let range= pvalue `div` 10 +1
    sign <- liftIO randomIO
    let pvalue' = max (pvalue + (range * if sign then 1 else -1)) 0
    
    setParameter name  pvalue'
    liftIO $ print ("pvalue",pvalue')
    return (name,pvalue)  !> (name,pvalue)


main= keep $ initNode $ inputNodes <|> local (optimizeProcess <|> process)

process= do 
   ths <- getParameter "number of threads" 20
   liftIO $ print ("new", ths)
   n <- threads ths $ choose  [1..]
   
   liftIO $ do atomicModifyIORef  counter $ \n -> (n+1,())

   
counter= unsafePerformIO $  newIORef (0 :: Int)

optimizeProcess= do 
   abduce 
   optimize $ liftIO $ do
    r <- readIORef counter   
    t <- getCPUTime
    threadDelay 1000000
    r' <- readIORef counter
    t' <- getCPUTime
    let ticks= fromIntegral $ (t'-t) `div`  1000000000
    let rr=  (r' - r)  `div`  ticks
    print ("counter",r'-r,ticks,rr)
    return $  rr

