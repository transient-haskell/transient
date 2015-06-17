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
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module Transient.Indeterminism (
choose, choose', collect, group --, found
) where

import Transient.Base
import Control.Monad.IO.Class
import Data.IORef
import Control.Applicative
import Data.Monoid
import Control.Concurrent
import Data.Typeable
import Control.Monad.State
import Control.Concurrent.STM as STM
import GHC.Conc


-- | slurp a list of values and process them in parallel . To limit the number of processing
-- threads, use `threads`
choose  ::  [a] -> TransientIO a
choose []= empty
choose   xs = do
    evs <- liftIO  $ newIORef xs
    parallel   $ do
           es <- atomicModifyIORef' evs $ \es -> let !tes= tail es in (tes,es)
           case es of
            [x]  -> return $ Left $ head es
            x:_  -> return $ Right x

-- | group the output of a possible mmultithreaded process in groups of n elements.
group :: Int -> TransientIO a -> TransientIO [a]
group num proc =  do
    v <- liftIO $ newIORef (0,[])
    x <- proc
    n <- liftIO $ atomicModifyIORef' v $ \(n,xs) -> let !n'=n +1 in ((n', x:xs),n')
    if n < num
      then stop
      else do
       liftIO $ atomicModifyIORef v $ \(n,xs) ->  ((0,[]),xs)

choose' :: [a] -> TransientIO a
choose'  xs = foldl (<|>) empty $ map (parallel . return . Left) xs


--newtype Collect a= Collect (MVar (Int, [a])) deriving Typeable

-- collect the results of a search done in parallel, usually initiated by
-- `choose` . The results are added to the collection with `found`
--
--


-- execute a process and get the first n solutions.
-- if the process end without finding the number of solutions requested, it return the fond ones
-- if he find the number of solutions requested, it kill the threads of the process and return
-- It works monitoring the solutions found and the number of active threads.
-- If the first parameter is 0, collect will return all the results
collect ::  Int -> TransientIO a -> TransientIO [a]
collect n search=  do
  rv <- liftIO $ atomically $ newTVar (0,[]) !> "NEWMVAR"
  endflag <- liftIO $ newTVarIO False
  st <- get
  let any1 = do
        r <- search   !> "ANY"
        liftIO $ atomically $ do
            (n1,rs) <- readTVar rv
            writeTVar  rv (n1+1,r:rs) !> "MODIFY"
        stop

      detect= do

        freeThreads $ do
          xs <- async $ do
             threadDelay 1000 -- to allow some activity before monitoring it
             atomically $ do
                (n',xs) <- readTVar rv
                ns <- readTVar $ children st

                if (n > 0 && n' >= n) ||  null ns  !> show (n,n') !> (show $ length ns)
                  then return xs
                  else retry

          th <- liftIO $ myThreadId !> "KILL"
          stnow <- get
          liftIO $ killChildren st
          liftIO $ addThread st stnow
          return  xs

  (any1 >> stop)  <|> detect

