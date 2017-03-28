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
{-# LANGUAGE  ScopedTypeVariables #-}
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


-- | slurp a list of values and process them in parallel . To limit the number of processing
-- threads, use `threads`
choose  :: Show a =>  [a] -> TransIO a
choose []= empty
choose   xs = do
    evs <- liftIO $ newIORef xs
    r <- parallel $ do
           es <- atomicModifyIORef' evs $ \es -> let tes= tail es in (tes,es)
           case es  of
            [x]  -> x `seq` return $ SLast x
            x:_  -> x `seq` return $ SMore x
    checkFinalize r

-- | alternative definition with more parallelism, as the composition of n `async` sentences
choose' :: [a] -> TransIO a
choose' xs = foldl (<|>) empty $ map (async . return) xs


-- | group the output of a possible multithreaded process in groups of n elements.
group :: Int -> TransIO a -> TransIO [a]
group num proc =  do
    v <- liftIO $ newIORef (0,[])
    x <- proc

    mn <- liftIO $ atomicModifyIORef' v $ \(n,xs) ->
            let n'=n +1
            in  if n'== num

              then ((0,[]), Just $ x:xs)
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
    mn <- liftIO $ atomicModifyIORef' v $ \(n,xs) -> let n'=n +1
            in
            if diffUTCTime t' t < fromIntegral time
             then ((n', x:xs),Nothing)
             else   ((0,[]), Just $ x:xs)
    case mn of
      Nothing -> stop
      Just xs -> return xs



-- collect the results of a search done in parallel, usually initiated by
-- `choose` .
--
-- execute a process and get at least the first n solutions (they could be more).
-- if he find the number of solutions requested, it kill the non-free threads of the process and return
collect ::  Int -> TransIO a -> TransIO [a]
collect n = collect' n 0

-- | search with a timeout
-- After the  timeout, it stop unconditionally and return the current results.
-- It also stops as soon as there are enough results specified in the first parameter.
-- The results are returned by the original thread
--
-- >     timeout t proc=do
-- >       r <- collect' 1 t proc
-- >       case r of
-- >          []  ->  empty
-- >          r:_ -> return r
--
-- >     timeout 10000 empty <|> liftIO (print "timeout")
--
-- That executes the alternative and will print "timeout".
-- This would not be produced if collect would not return the results to the original thread
--
-- `search` is executed in different threads and his state is lost, so don't rely in state
-- to pass information

collect' :: Int -> Int -> TransIO a -> TransIO [a]
collect' n t search= do

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




