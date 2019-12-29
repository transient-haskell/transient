#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
-- LIB="/projects" && runghc     -i${LIB}/transient/src -i${LIB}/transient-universe/src -i${LIB}/axiom/src   $1 ${2} ${3}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

--import Transient.Move
--import Transient.Move.Utils
--import Transient.Logged
import Transient.Internals
import Transient.Indeterminism
--import Transient.EVars
--import Network
import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Data.Monoid
import System.IO
import Control.Monad
import Data.Maybe
import Control.Exception hiding (onException)
import Data.Typeable
import Data.IORef
import Data.List((\\))






-- async exceptions
main1 = keep $ job <|> killer

job= do
        abduce
        labelState "JOB"
        onException $ \(e :: SomeException) ->  do 
                th <- liftIO myThreadId
                liftIO $ print ("JOB", e,th)  
                empty
        tmask $ liftIO $ print (sum [0..10000000 :: Int]) 
        
        liftIO $ Main.loop [0..] "JOB"

loop [] _   = return()
loop xs msg = do       
        threadDelay 1000000
        print msg
        Main.loop (tail xs) msg

killer = do
        liftIO $ threadDelay 1000000
        th <- threadState "JOB"
        liftIO $ throwTo th  $ ErrorCall "hello"


killer2 = do
    abduce
    labelState "KILLER"

    onException $ \(e :: SomeException) ->  do 
          th <- liftIO myThreadId
          liftIO $ print ("KILLER", e,th)
          empty

    liftIO $ threadDelay 5000000
    st <- getCont

    liftIO $ killChildren $ children $ fromJust $ parent  st
    liftIO $ Main.loop [0..] "KILLER"
    return ()


tmask :: TransIO a -> TransIO a
tmask proc = do
        (mr,_) <- liftIO $ mask_ $ runTransient proc
        if isJust mr then return $ fromJust mr else empty


withResource f= do
        print "open resource"
        mv <- newMVar "hello"
        r <- takeMVar mv
        f r
        print "close resource"
        putMVar mv r
        
main= keep $ do
        collect 1 $ do
                r <- react withResource (return ()) 
                liftIO $ print r  
        liftIO $ print "world"

