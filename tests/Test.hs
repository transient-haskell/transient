{-# LANGUAGE   CPP, ScopedTypeVariables, DeriveDataTypeable, RebindableSyntax #-}

import qualified Prelude as Pr(return)
import Prelude hiding ((>>=),(>>),return)
import Transient.TypeLevel.Effects
import Transient.TypeLevel.Base
import Transient.TypeLevel.EVars
import Transient.TypeLevel.Indeterminism

import Transient.Logged
import Data.Typeable
import Control.Applicative
import Data.Monoid

import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)

import System.Directory
import System.IO
import System.Random
import Control.Exception
import Control.Concurrent.MVar 
import Control.Concurrent

import Data.String



main= keep $ do
  r <- async $ Pr.return "hola"
  x <- waitEvents $ Pr.return $  "hello" 
  liftIO $ print (r,x)

{-
main2= keep $ do
   v <- newEmptyMVar
   forkIO $ doasync >>= writeMVar v
   r <- readMVar v
   return r
   
   future <- async doasync
   r <- wait future
   
   r <- async doasync <|> async doasync2

   
main1= keep $ do
   
   oneThread $ return ()


   (async (threadDelay 1000) >> labelState (fromString "abduce") )  <|>  return () -- ( topState >>= showThreads >>return ())
   abduce
   labelState $ fromString "hello"
   topState >>= showThreads
    -}
