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
choose,  choose', chooseStream, collect, collect', group, groupByTime, burst
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




-- | Converts a list of pure values into a transient task set. You can use the
-- 'threads' primitive to control the parallelism.
--
choose  ::  [a] -> TransIO  a
choose []= empty
choose   xs = chooseStream xs >>= checkFinalize  

-- | transmit the end of stream
chooseStream  ::  [a] -> TransIO (StreamData a)
chooseStream []= empty
chooseStream   xs = do
    evs <- liftIO $ newIORef xs
    parallel $ do
           es <- atomicModifyIORef evs $ \es -> let tes= tail es in (tes,es)
           case es  of
            [x]  -> x `seq` return $ SLast x
            x:_  -> x `seq` return $ SMore x


-- | Same as 'choose',  slower in some cases
--
choose' :: [a] -> TransIO a
choose' xs = foldl (<|>) empty $ map (async . return) xs


-- | Collect the results of a task set in groups of @n@ elements.
--
group :: Int -> TransIO a -> TransIO [a]
group num proc =  do
    v <- liftIO $ newIORef (0,[])
    x <- proc

    mn <- liftIO $ atomicModifyIORef v $ \(n,xs) ->
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

{-
groupByTime1 time proc =  do
    t  <- liftIO getCurrentTime

    v  <- liftIO $ newIORef (0,t,[])
    
    x  <- proc
    t' <- liftIO getCurrentTime
    mn <- liftIO $ atomicModifyIORef v $ \(n,t,xs) -> let n'=n +1
            in
            if diffUTCTime t' t < fromIntegral time
             then   ((n',t, x:xs),Nothing)
             else   ((0 ,t',[]), Just $ x:xs)
    case mn of
      Nothing -> stop
      Just xs -> return xs
-}

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


  rv <- liftIO $ newEmptyMVar     -- !> "NEWMVAR"

  results <- liftIO $ newIORef (0,[])

  let worker =  do
        r <- abduce >> search
        liftIO $  putMVar rv $ Just r
        stop

      timer= do
             when (t > 0) $ do
                addThreads 1
                async $ threadDelay t >> putMVar rv Nothing 
             empty

      monitor=  liftIO loop 

          where
          loop = do
                mr <- takeMVar rv

                (n',rs) <- readIORef results
                case mr of
                  Nothing -> return rs
                  Just r -> do
                     let n''= n' + 1
                     let rs'= r:rs
                     writeIORef results  (n'',rs')

                     t' <-  getCurrentTime
                     if (n > 0 && n'' >= n)
                       then  return (rs')
                       else loop
              `catch` \(e :: BlockedIndefinitelyOnMVar) -> 
                                   readIORef results >>= return . snd


  oneThread $  timer <|> worker <|> monitor 


-- | insert `SDone` response every time there is a timeout since the last response

burst :: Int -> TransIO a -> TransIO (StreamData a)
burst timeout comp= do
     r <- oneThread comp 
     return (SMore r) <|> (async (threadDelay timeout) >> return SDone)
     
groupByTime :: Monoid a => Int -> TransIO a -> TransIO a
groupByTime timeout comp= do
     v <- liftIO $ newIORef mempty 
     gather v <|> run v 
     where
     run v =  do 
        x <-  comp
        liftIO $ atomicModifyIORef v $ \xs -> (xs <> x,())
        empty
        
     gather v= waitEvents $ do
             threadDelay timeout 
             atomicModifyIORef v $ \xs -> (mempty , xs) 


   
 