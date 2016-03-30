{-# LANGUAGE   CPP #-}

module Main where

import Prelude hiding (div)
import Transient.Base
#ifdef ghcjs_HOST_OS
   hiding ( option,runCloud')
#endif
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map)
#else
   hiding (map, option,runCloud')
#endif

import  Transient.Move  hiding(teleport)
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

main = do
  let serverAddr= "localhost"
      serverPort =  2020
      serverNode  = createNode serverAddr serverPort
      mynode    = if isBrowserInstance
                     then createWebNode
                     else serverNode
  runCloud' $  do
        listen mynode

        wormhole serverNode $  widget  <|> widget

widget =  do

         op <-  local $ render $   (inputSubmit "start"  `fire` OnClick)
                               <|> (inputSubmit "cancel" `fire` OnClick) <++ br

         teleport          -- translates the computation to the server

         r <- local $ case op of
                   "start"  ->  stream
                   "cancel" ->  killChilds >> empty

         teleport          -- back to the browser again

         local $ render $ rawHtml $ h1 r

-- generates a sequence of numbers
stream= do
     counter <- liftIO $ newIORef (0 :: Int)
     waitEvents $ do
          n <- atomicModifyIORef counter $ \r -> (r +1,r)
          threadDelay 1000000
          putStr "generating: " >> print n
          return  n



