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
module Transient.Stream.Resource(sourceFile, sinkFile, process, finish, onFinish) where


import Transient.Base hiding (loop)
import Transient.EVars
import Control.Exception
import Control.Applicative
import Control.Monad.IO.Class
import Data.Typeable
import Data.Char
import System.IO






-- | Stream the input to a file
sinkFile :: TransIO String -> String -> TransIO ()
sinkFile input file= process input (openFile file WriteMode)  hClose' hPutStrLn'
  where
  hClose' h= putStr "closing " >> putStrLn file >> hClose h
  hPutStrLn' h  x= liftIO $ (SMore <$>  hPutStrLn h x)
                  `catch` (\(e::SomeException)-> return $ SError (show e))

-- | slurp input from a file a line at a time. It creates as much threads as possible.
-- to allow single threaded processing, use it with `threads 0`
sourceFile :: String -> TransIO String
sourceFile file= process (return ()) (openFile file ReadMode)  hClose' read'
      where
      hGetLine' h= (SMore <$> hGetLine h)
                   `catch` (\(e::SomeException)-> return $ SError(show e))
      read' h _ =  parallel $ hGetLine' h


      hClose' h= putStr "closing ">> putStrLn file >> hClose h

-- | is the general operation for processing a streamed input, with opening  resources before
-- processing and closing them when finish is called.  The process statements suscribe to the
-- EVar `Finish`.
--
-- when this variable is updated, the close section is called.
--
-- When the processing return `SDone` or `SError`, the `Finish` variable is updated so all the
-- subscribed code that close the resources are executed.
process
  :: TransIO a
     -> IO handle
     -> (handle -> IO ())
     -> (handle -> a -> TransIO (StreamData b))
     -> TransIO b
process input open close process=do
   mh <- liftIO $ (Right <$> open)  `catch` (\(e::SomeException)-> return $ Left e)
   case mh of
      Left e -> liftIO (putStr "process: " >> print e) >> finish  >> stop
      Right h -> do
       onFinish (liftIO (close h) >> killChilds >> stop) <|> return()
       some <- input
       process' h  some
       where
       process' h something = do
           v <- process h  something
           checkFinalize v



checkFinalize v=
           case v of
              SDone ->  finish  >> stop
              SLast x ->  finish >> return x
              SError e -> liftIO ( putStrLn e) >> finish  >> stop
              SMore x -> return x



newtype Finish= Finish (EVar Bool) deriving Typeable

initFinish :: TransIO Finish
initFinish= do
      fin <- newEVar
      let f = Finish fin
      setSData  f
      return f

-- | suscribe a computation to be called when the finish event is triggered
onFinish :: TransIO () -> TransIO a
onFinish  close= do
       Finish finish <- getSData <|> initFinish
       readEVar finish
       close
       stop

-- | trigger the event for the closing of all the resources
finish :: TransIO ()
finish = do
    liftIO $ putStrLn "finish Called"
    Finish finish <- getSData
    writeEVar finish True







