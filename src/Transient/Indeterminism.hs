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

(**>) x y=   do
       Transient $ runTrans x
       Transient $ runTrans y

(<**) x y= do
       r <- Transient $ runTrans x
       Transient $ runTrans y
       return r

collect :: Typeable a => Int -> Int -> TransientIO a -> TransientIO [a]
collect n time any=  do
  rv <- liftIO $ atomically $ newTVar (0,[]) !> "NEWMVAR"
  endflag <- liftIO $ newTVarIO False
  st <- get
  let any1 = do
        r <- any   !> "ANY"
        liftIO $ atomically $ do
            (n1,rs) <- readTVar rv
            writeTVar  rv (n1+1,r:rs) !> "MODIFY"
--        liftIO $ atomically $ writeTVar endflag True   !> "ENDFLAG"

      detect=  do
        stnow <- get
        Transient $ liftIO $ do


         mxs <- atomically $ do
            (n',xs) <- readTVar rv  !> "TAKE"
            if n' < n
             then do
                writeTVar rv (n',xs)
                return Nothing
             else do
                writeTVar rv (0,[])
                return $ Just xs
         case mxs of
            Nothing -> return Nothing
            Just xs -> do
                th <- myThreadId
                free th stnow
                killChildren st
                return $ Just xs


      timeout = do
        stnow <- get
        async $ do

          (n',xs) <- atomically $ do
                (n',xs) <-  readTVar rv !> "READTVAR"
                when (n'== 0) STM.retry
                unsafeIOToSTM $  threadDelay time
                return (n',xs)

          th <- liftIO $ myThreadId
          free th stnow    !> "FREE"
          killChildren st  !> "KILL"
          return xs

  any1 **> detect   <|> timeout

free th env= do
       let sibling= children $ parent env
       sbs <- readIORef sibling
       if th `elem` sbs
         then
            sibling =: \sb ->  filter (/= th) sb  -- remove this thread from the list
         else
            free th $ parent env

--found ::  Typeable a => a -> TransientIO ()
--found x= do
--     mcv <- getSessionData
--     case mcv of
--         Nothing ->  error "found: out of collect block"
--         Just (Collect tv) ->  liftIO $  do
--           modifyMVar_ tv $ \(n,xs) ->  return (n+1, x:xs)

