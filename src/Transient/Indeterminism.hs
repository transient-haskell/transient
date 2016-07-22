-----------------------------------------------------------------------------
--
-- Module      :  Transient.Indeterminism
-- Copyright   :
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | see <https://www.fpcomplete.com/user/agocorona/beautiful-parallel-non-determinism-transient-effects-iii>
--
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Transient.Indeterminism (
choose, choose', collect, collect', group, groupByTime
) where

import Transient.Base
import Transient.EVars
import Transient.Internals(killChildren, EventF(..),hangThread)
import Data.IORef
import Control.Applicative
import Data.Monoid
import Control.Concurrent
import Data.Typeable
import Control.Monad.State
import Control.Concurrent.STM as STM
import GHC.Conc
import Data.Time.Clock


-- | slurp a list of values and process them in parallel . To limit the number of processing
-- threads, use `threads`
choose  :: Show a =>  [a] -> TransIO a
choose []= empty
choose   xs = do
    evs <- liftIO $ newIORef xs
    r <- parallel $ do
           es <- atomicModifyIORef' evs $ \es -> let !tes= tail es in (tes,es)
           case es  of
            [x]  -> x `seq` return $ SLast x
            x:_  -> x `seq` return $ SMore x
    toData r

toData r= case r of
      SMore x -> return x
      SLast x -> return x
      SError e ->  finish (Just e) >> empty


-- | group the output of a possible multithreaded process in groups of n elements.
group :: Int -> TransIO a -> TransIO [a]
group num proc =  do
    v <- liftIO $ newIORef (0,[])
    x <- proc

    mn <- liftIO $ atomicModifyIORef' v $ \(n,xs) ->
            let !n'=n +1
            in  if n'== num

              then ((0,[]), Just xs)
              else ((n', x:xs),Nothing)
    case mn of
      Nothing -> stop
      Just xs -> return xs

-- | group result for a time interval, measured with `diffUTCTime`
groupByTime :: Integer -> TransIO a -> TransIO [a]

groupByTime time proc =  do
    v  <- liftIO $ newIORef (0,[])
    t  <- liftIO getCurrentTime
    x  <- proc
    t' <- liftIO getCurrentTime
    mn <- liftIO $ atomicModifyIORef' v $ \(n,xs) -> let !n'=n +1
            in
            if diffUTCTime t' t < fromIntegral time
             then ((n', x:xs),Nothing)
             else   ((0,[]), Just xs)
    case mn of
      Nothing -> stop
      Just xs -> return xs

-- | alternative definition with more parallelism, as the composition of n `async` sentences
choose' :: [a] -> TransIO a
choose' xs = foldl (<|>) empty $ map (async . return) xs


--newtype Collect a= Collect (MVar (Int, [a])) deriving Typeable

-- collect the results of a search done in parallel, usually initiated by
-- `choose` . The results are added to the collection with `found`
--
--


-- | execute a process and get at least the first n solutions (they could be more).
-- if the process end without finding the number of solutions requested, it return the found ones
-- if he find the number of solutions requested, it kill the non-free threads of the process and return
-- It works monitoring the solutions found and the number of active threads.
-- If the first parameter is 0, collect will return all the results
collect ::  Int -> TransIO a -> TransIO [a]
collect n = collect' n 0.1 0

-- | search also between two time intervals. If the first interval has passed and there is no result,
--it stops.
-- After the second interval, it stop unconditionally and return the current results.
-- It also stops as soon as there are enough results specified in the first parameter.
collect' :: Int -> NominalDiffTime -> NominalDiffTime -> TransIO a -> TransIO [a]
collect' n t1 t2 search= hookedThreads $  do
  rv <- liftIO $ atomically $ newTVar (0,[]) -- !> "NEWMVAR"
  endflag <- liftIO $ newTVarIO False
  st <-  get
  t <- liftIO getCurrentTime
  let worker = do
        r <- search    -- !> "ANY"
        liftIO $ atomically $ do
            (n1,rs) <- readTVar rv
            writeTVar  rv (n1+1,r:rs)  -- !> "MODIFY"
        stop

      monitor=  freeThreads $ do
          xs <- async $ atomically $
                          do (n', xs) <- readTVar rv
                             ns <- readTVar $ children st
                             t' <- unsafeIOToSTM getCurrentTime
                             if
                               (n > 0 && n' >= n) ||
                                 (null ns && (diffUTCTime t' t > t1))    ||
                                 (t2 > 0 && diffUTCTime t' t > t2)
                                        -- !>  (diffUTCTime t' t, n', length ns)
                               then return xs else retry

          th <- liftIO $ myThreadId   -- !> "KILL"
          stnow <-  get
          liftIO . killChildren $ children st
          liftIO $ hangThread st stnow
          return  xs

  monitor <|> worker




