{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable,  CPP #-}


module Main where

import Prelude hiding (div)
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


import  Transient.Move  hiding(teleport)
import Transient.Logged
import Transient.Stream.Resource

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

import Control.Concurrent.STM





main = runCloud $  do
    args <- onAll $ liftIO getArgs

    let serverPort =  2020 -- read (args !! 0)

        serverNode  = createNode "localhost" serverPort
        mynode    = if isBrowserInstance
                       then  createWebNode
                       else createNode "localhost" serverPort

--    connect mynode serverNode

    listen mynode


    wormhole serverNode $ do
       widget  <|> widget




widget =  do
         op <-   local $ render $  ( inputSubmit "start"  `fire` OnClick)
                               <|> ( inputSubmit "cancel" `fire` OnClick) <++ br
         teleport          -- translates the computation to the server
         context <- get
         r <- local $ case op of
                   "start"  ->  sequ
                   "cancel" ->  stopContext context >> empty

         teleport          -- back to the browser again


         local  $ render $ rawHtml $ h1 r

stopContext cont= liftIO $ killChildren cont


-- generates a sequence of numbers
sequ= do
     counter <- liftIO $ newIORef (0 :: Int)
     waitEvents $ do
          n <- atomicModifyIORef counter $ \r -> (r +1,r)
          threadDelay 1000000
          putStr "generating: " >> print n
          return  n



