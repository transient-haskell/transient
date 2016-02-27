{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable,  CPP #-}


module Main where

import Transient.Base
#ifdef ghcjs_HOST_OS
   hiding ( option,runCloud)
#endif
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map)
#else
   hiding (map, option,runCloud)
#endif


import Transient.Move
import Transient.Logged
import Transient.Stream.Resource
import Control.Monad.IO.Class
import Control.Applicative

import System.Environment
import Control.Monad
import Control.Concurrent

import Transient.Indeterminism
import Data.Monoid
import Data.Typeable


import Data.IORef
import Control.Monad.State
import Control.Concurrent

import System.IO.Unsafe




main = runCloud $  do
    args <- cexec $ liftIO getArgs

    let localPort =  2020 -- read (args !! 0)
        serverPort  = 2020 -- read (args !! 1)

--
        serverNode  = createNode "localhost" serverPort
        mynode    = if isBrowserInstance
                       then  WebNode  (unsafePerformIO emptyPool)
                       else createNode "localhost" localPort

--    connect mynode serverNode

    listen1 mynode serverNode




    render $   widget  <|>  widget


widget =  do
         local addPrefix
         op <-  local  $ option "fire" "fire"   <|>  option "cancel" "cancel"

         teleport          -- translates the execution to the server

         context <- get
         r <- local $ case op of
                   "fire" ->    stopContext context >> sequ
                   "cancel" ->  stopContext context >> empty

         teleport          -- back to the browser again

         local $ rawHtml $ h1 r

stopContext cont= liftIO $ killChildren cont


-- generates a sequence of numbers
sequ= do

     counter <- liftIO $ newIORef (0 :: Int)
     waitEvents $ do
          n <- atomicModifyIORef counter $ \r -> (r +1,r)
          threadDelay 1000000
          putStr "generating: " >> print n
          return  n


