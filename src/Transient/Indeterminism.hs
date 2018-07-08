-----------------------------------------------------------------------------
--
-- Module      :  Transient.Indeterminism
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | see <https://www.fpcomplete.com/user/agocorona/beautiful-parallel-non-determinism-transient-effects-iii>
--
-----------------------------------------------------------------------------
{-# LANGUAGE  ScopedTypeVariables, CPP #-}
module Transient.Indeterminism (
choose, choose', collect, collect', group, groupByTime
) where

import Transient.Internals hiding (retry)

import Data.IORef
import Control.Applicative
import Data.Monoid
import Control.Concurrent 
import Data.Typeable
import Control.Monad.State
import GHC.Conc
import Data.Time.Clock
import Control.Exception 

#ifndef ETA_VERSION
import Data.Atomics
#endif 


-- | Converts a list of pure values into a transient task set. You can use the
-- 'threads' primitive to control the parallelism.
--
choose  :: Show a =>  [a] -> TransIO a
choose []= empty
choose   xs = do
    evs <- liftIO $ newIORef xs
    r <- parallel $ do
           es <- atomicModifyIORefCAS evs $ \es -> let tes= tail es in (tes,es)
           case es  of
            [x]  -> x `seq` return $ SLast x
            x:_  -> x `seq` return $ SMore x
    checkFinalize r

-- | Same as 'choose' except that the 'threads' combinator cannot be used,
-- instead the parent thread's limit applies.
--
choose' :: [a] -> TransIO a
choose' xs = foldl (<|>) empty $ map (async . return) xs


-- | Collect the results of a task set in groups of @n@ elements.
--
group :: Int -> TransIO a -> TransIO [a]
group num proc =  do
    v <- liftIO $ newIORef (0,[])
    x <- proc

    mn <- liftIO $ atomicModifyIORefCAS v $ \(n,xs) ->
            let n'=n +1
            in  if n'== num

              then ((0,[]), Just $ x:xs)
              else ((n', x:xs),Nothing)
    case mn of
      Nothing -> stop
      Just xs -> return xs

-- | Collect the results of a task set, grouping all results received within
-- every time interval specified by the first parameter as `diffUTCTime`.
--
groupByTime :: Integer -> TransIO a -> TransIO [a]

groupByTime time proc =  do
    t  <- liftIO getCurrentTime

    v  <- liftIO $ newIORef (0,t,[])
    
    x  <- proc
    t' <- liftIO getCurrentTime
    mn <- liftIO $ atomicModifyIORefCAS v $ \(n,t,xs) -> let n'=n +1
            in
            if diffUTCTime t' t < fromIntegral time
             then   ((n',t, x:xs),Nothing)
             else   ((0 ,t',[]), Just $ x:xs)
    case mn of
      Nothing -> stop
      Just xs -> return xs


-- | Collect the results of the first @n@ tasks.  Synchronizes concurrent tasks
-- to collect the results safely and kills all the non-free threads before
-- returning the results.  Results are returned in the thread where 'collect'
-- is called.
--
collect ::  Int -> TransIO a -> TransIO [a]
collect n = collect' n 0

-- | Like 'collect' but with a timeout. When the timeout is zero it behaves
-- exactly like 'collect'. If the timeout (second parameter) is non-zero,
-- collection stops after the timeout and the results collected till now are
-- returned.
--
collect' :: Int -> Int -> TransIO a -> TransIO [a]
collect' n t search= do
  addThreads 1
  rv <- liftIO $ newEmptyMVar     -- !> "NEWMVAR"

  results <- liftIO $ newIORef (0,[])

  let worker =  do
        r <- abduce >> search
        liftIO $  putMVar rv $ Just r
        stop

      timer= do
             when (t > 0) . async $ threadDelay t >>putMVar rv Nothing -- readIORef results  >>= return . snd
             empty

      monitor=  liftIO loop

          where
          loop = do
                mr <- takeMVar rv
                (n',rs) <- readIORef results
                case mr of
                  Nothing -> return rs
                  Just r -> do
                     let n''= n' +1
                     let rs'= r:rs
                     writeIORef results  (n'',rs')

                     t' <-  getCurrentTime
                     if (n > 0 && n'' >= n)
                       then  return (rs')
                       else loop
              `catch` \(e :: BlockedIndefinitelyOnMVar) -> 
                                   readIORef results >>= return . snd


  r <- oneThread $  worker <|> timer <|> monitor

  return r




