{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, MonadComprehensions
, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Main where

import Transient.Move
import Transient.Logged
import Transient.Base hiding (loop)
import Transient.EVars
import Transient.Indeterminism
import Network
import Network.Socket hiding (listen, accept,Stream)
import Control.Applicative

import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Data.Monoid
import System.IO
import Control.Monad
import Data.Maybe
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Typeable
import Control.Concurrent.STM
import Data.IORef
import Control.Monad.State

import Control.Concurrent.STM
import Data.IORef
import Control.Monad.State
import Unsafe.Coerce


import Data.Char

import Control.Concurrent

import qualified Data.ByteString.Char8 as BS
import System.IO
import Foreign.Ptr
import Foreign.Storable
import Data.ByteString.Internal
import Foreign.ForeignPtr.Safe
import System.Time



-- newtype TStream a= TStream  (StreamData a) deriving (Typeable, Read, Show)

-- data StreamExp= forall a b. StreamExp[a ->TransIO (TStream b)] deriving Typeable

main= keep $ sourceFile "../transient.cabal" >>= \chunk -> return (map toUpper chunk) `sinkFile` "out"


sinkFile :: TransIO String -> String -> TransIO ()
sinkFile input file= process input (openFile file WriteMode)  hClose' hPutStrLn'
  where
  hClose' h= putStr "closing " >> putStrLn file >> hClose h
  hPutStrLn' h  x= (SMore <$>  hPutStrLn h x)
                  `catch` (\(e::SomeException)-> return $ SError (show e))


process
  :: TransIO a
     -> IO handle
     -> (handle -> IO ())
     -> (handle -> a -> IO (StreamData b))
     -> TransIO b
process input open close process=do
   h <- liftIO open
   onFinish (liftIO (close h) >> stop) <|> return()
   some <- input
   process' h  some
   where
   process' h something = do
       v <-  liftIO $ process h  something
       checkFinalize v

sourceFile :: String -> TransIO String
sourceFile file= source (openFile file ReadMode)  hClose' hGetLine'
      where
      hGetLine' h= (SMore <$> hGetLine h)
                   `catch` (\(e::SomeException)-> return $ SError(show e))

      hClose' h= putStr "closing ">> putStrLn file >> hClose h

source
  :: IO handle -> (handle -> IO a) -> (handle -> IO (StreamData b)) -> TransIO b
source open close read= do
       h <- liftIO open
       onFinish (liftIO (close h) >>stop) <|> return()
       read' h
       where
       read' h  = do
         v <-  parallel $ read h
         checkFinalize v

--       par ::  IO  (StreamData b) -> TransIO b
       par proc= do
          r <- parallel proc
          checkFinalize r

checkFinalize v=
           case v of
              SDone ->  finish  >> stop
              SLast x ->  finish >> return x
              SError e -> liftIO (putStr "slurp: " >> putStrLn e) >> finish  >> stop
              SMore x -> return x



newtype Finish= Finish (EVar Bool) deriving Typeable

initFinish :: TransIO Finish
initFinish= do
      fin <- newEVar
      let f = Finish fin
      setSData  f
      return f

onFinish :: TransIO () -> TransIO a
onFinish  close= do
       Finish finish <- getSData <|> initFinish
       readEVar finish
       close
       stop

finish :: TransIO ()
finish = do
    liftIO $ putStrLn "finish Called"
    Finish finish <- getSData
    writeEVar finish True




