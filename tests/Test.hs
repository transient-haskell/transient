{-# LANGUAGE   CPP, OverloadedStrings,ScopedTypeVariables, DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}

import Transient.Internals
import Transient.EVars
import Transient.Logged
import Transient.Parse
import Transient.Indeterminism
import Data.Typeable
import Control.Applicative
import Data.Monoid

import System.Directory
import System.IO
import System.Random
import Control.Exception
import Control.Concurrent.MVar 
import Control.Concurrent

import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Builder
import Control.Monad.IO.Class
import System.Time
import Control.Monad
-- import Data.Beamable hiding (serialize,deserialize)

instance Loggable BS.ByteString
instance Loggable Int  where
   serialize = intDec -- byteString . toBSSign       
   deserialize= withGetParseString $ \str -> 
           case BS.readInt str of   -- = int  readInt
             Just  x -> return  x
             Nothing -> empty
             
             
             
main= keep' $ test4 

test4= do
    sync (do (async $ do threadDelay 1000000; print "hello") ) <|> liftIO ( print "word")

   
test3=  noTrans $ do
    r <- runTrans $ collect 1 $ choose' ["hello","world" :: String]
    liftIO $ print r
    
test1= do
   t1 <- liftIO $ getClockTime 
   sum <- foldM  (\sum i -> do
      setParseString $  toLazyByteString $ serialize (i:: Int)
      s <- deserialize
      return $ sum+ s)
      0 [1..1000]
   t2 <- liftIO $ getClockTime
   liftIO $ print (sum :: Int)
   
   
test2= do
  t1 <- liftIO $ getClockTime 
  forM [1..1000] $ \i -> do
     logged $ return (i :: Int)
  Just(Log _ _ full _ _) <- getData
  -- liftIO $ print $ toLazyByteString full
  setData $ Log True full full 0 0
  setParseString $ toLazyByteString full
  sum <- foldM  (\sum i -> do
      s <- logged $ return (0:: Int)
      -- liftIO $ print s
      return $ sum+ s)
      0 [1..1000]

  t2 <- liftIO getClockTime
  liftIO $ print (sum :: Int)
  liftIO $ print  $ diffClockTimes  t2 t1
  
   