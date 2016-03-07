{-# LANGUAGE   CPP #-}


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
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

main = do
  args <- getArgs
  serverAddr= if length args > 0 then  read (args !! 0) else "localhost"



  let serverPort =  2020

      serverNode  = createNode serverAddr serverPort
      mynode    = if isBrowserInstance
                     then createWebNode
                     else serverNode
  runCloud $  do
--    connect mynode serverNode

    listen mynode

    wormhole serverNode $  widget  <|> widget

widget =  do
         op <-   local $ render $  ( inputSubmit "start"  `fire` OnClick)
                               <|> ( inputSubmit "cancel" `fire` OnClick) <++ br
         teleport          -- translates the computation to the server

         r <- local $ case op of
                   "start"  ->  sequ
                   "cancel" ->  killChilds >> empty

         teleport          -- back to the browser again

         local  $ render $ rawHtml $ h1 r

-- generates a sequence of numbers
sequ= do
     counter <- liftIO $ newIORef (0 :: Int)
     waitEvents $ do
          n <- atomicModifyIORef counter $ \r -> (r +1,r)
          threadDelay 1000000
          putStr "generating: " >> print n
          return  n



