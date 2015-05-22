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

module Transient.Indeterminism (
choose, collect
) where

import Transient.Base
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Applicative

-- | slurp a list of values and process them in parallel . To limit the number of processing
-- threads, use `threads`
choose  ::  [a] -> TransientIO a
choose   xs = freeThreads $ do
    evs <- liftIO  $ newMVar xs  !> "NEWMVAR"
    parallel   $ do
           es <- takeMVar evs
           putMVar evs $ tail es   !> "NEW VALUE"
           case es of
            [x]  -> return $ Left $ head es
            x:_  -> return $ Right x  !> "FINISH LIST"

-- | group the output of a possible mmultithreaded process in groups of n elements.
collect :: Int -> TransientIO a -> TransientIO [a]
collect num proc =  do
    v <- liftIO $ newMVar (0,[])
    x <- proc
    n <- liftIO $ modifyMVar v $ \(n,xs) -> let n'=n +1 in return $ ((n', x:xs),n')
    if n < num
      then stop
      else do
       liftIO $ modifyMVar v $ \(n,xs) -> return ((0,[]),xs)

