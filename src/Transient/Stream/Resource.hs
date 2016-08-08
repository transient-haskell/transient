-----------------------------------------------------------------------------
--
-- Module      :  Transient.Stream.Resource
-- Copyright   :
-- License     :  GPL-3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable  #-}
module Transient.Stream.Resource(sourceFile, sinkFile, process, initFinish, finish, onFinish) where


import Transient.Base hiding (loop)
import Transient.Backtrack
import Control.Exception
import Control.Applicative
import Data.Typeable
import Data.Char
import System.IO

import Control.Concurrent

import Control.Concurrent.STM
import Control.Monad.State



-- | Stream the input to a file
sinkFile :: TransIO String -> String -> TransIO ()
sinkFile input file= process input (openFile file WriteMode)  hClose' hPutStrLn'
  where
  hClose' h _= putStr "closing " >> putStrLn file >> hClose h
  hPutStrLn' h  x= liftIO $ (SMore <$>  hPutStrLn h x)
                  `catch` (\(e::SomeException)-> return $ SError e)

-- | slurp input from a file a line at a time. It creates as much threads as possible.
-- to allow single threaded processing, use it with `threads 0`
sourceFile :: String -> TransIO String
sourceFile file= process (return ()) (openFile file ReadMode)  hClose' read'
      where
      hGetLine' h= (SMore <$> hGetLine h)
                   `catch` (\(e::SomeException)-> return $ SError e)
      read' h _ =  parallel $ hGetLine' h


      hClose' h _= putStr "closing ">> putStrLn file >> hClose h

-- | is the general operation for processing a streamed input, with opening  resources before
-- processing and closing them when finish is called.  The process statements suscribe to the
-- `Finish` EVar.
--
-- When this variable is updated, the close procedure is called.
--
-- When the processing return `SDone` or `SError`, the `Finish` variable is updated so all the
-- subscribed code, that close the resources, is executed.
process
  :: TransIO a       -- ^ input computation
     -> IO handle    -- ^ open computation that gives resources to be used during the computation
     -> (handle -> FinishReason -> IO ())   -- ^ close computation that frees the resources
     -> (handle -> a -> TransIO (StreamData b))   -- ^ process to be done
     -> TransIO b
process input open close proc=do
   mh <- liftIO $ (Right <$> open)  `catch` (\(e::SomeException)-> return $ Left e)
   case mh of
      Left e -> liftIO (putStr "process: " >> print e) >> finish  (Just e) >> stop
      Right h -> do
       onFinish (liftIO . close h)
       some <- input
       v <- proc h  some
       liftIO $ myThreadId >>= print
       checkFinalize v
