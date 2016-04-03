{-# LANGUAGE   CPP #-}

module Main where

import Prelude hiding (div,id,span)
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

import Transient.Move
import Transient.Indeterminism
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class




main = simpleWebApp 2020 $  demo <|> demo2  <|>  counters


demo= do
   name <- local . render $ do
       rawHtml $ do
          p "this snippet captures the essence of this demonstration"
          p $ span "it's a blend of server and browser code in a "
               >> (span $ b "composable") >> span " piece"

          div ! id (fs "fibs") $ i "Fibonacci numbers should appear here"

   local . render $ wlink () (p " stream fibonacci numbers")

   -- stream fibonancci

   r <-  atServer $ do
               let fibs=  0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]  --fibonacci numb. definition

               r <- local  $ return $ take 10 fibs
               lliftIO $ print r
               lliftIO $ threadDelay 1000000
               return r

   local . render . at (fs "#fibs") Append $ rawHtml $  (h2 r)



demo2= do
   name <- local $ render $ inputString (Just "Your name") `fire` OnKeyUp <++ br

   r <- atServer $ lliftIO $ print (name ++ " calling") >> return ("Hi " ++ name)

   local . render . rawHtml $ do
            p " returned"
            h2 r



fs= toJSString

{-





demo1 = do
-- render some text
   local $ render $ do
     rawHtml $ do
          br
          div ! id (fs "fibs") $ i "Fibonacci numbers should appear here"

          br
          p "Hi, this is a demo of some features of Transient for Web applications"

     wlink () (p $ toElem "send something " >> b "to the server") <++ br

   name <- local $ render $ inputString "Your name" `fire` OnClick
   r <- atServer $ lliftIO $ print (name ++ "calling") >> return "Hi"

   local $ render $ rawHtml $ do
            p " returned"
            h2 r

   local .  render $ wlink () (p " stream fibonacci numbers")

   -- stream fibonancci

   r <-  atServer $ do
               let fibs=  0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]
               r <- local . threads 1 . choose $ take 10 fibs
               lliftIO $ threadDelay 1000000
               return r

   local . render $ at (fs "#fibs") Append $ rawHtml $ span r

   stop






-- To demonstrate wormhole, teleport, widgets, interactive streaming
-- and composability in a web application.
--
-- This is one of the most complicated interactions: how to control a stream in the server
-- by means of a web interface without loosing composability.
--
-- in this example, events flow from the server to the browser (a counter) and back from
-- the browser to the server (initiating and cancelling the counters)
counters= do
   serverNode <- onAll getSData <|> error "no server node" :: Cloud Node
--   local . render $ inputSubmit "show counters"  `fire` OnClick <++ br
   wormhole serverNode $ counter <|> counter

-}

counters= do
   server <- local $ getSData <|> error "no server???"

   wormhole  server $ counter <|> counter

counter  =  do

         op <-  startOrCancel
         teleport          -- translates the computation to the server
         r <- local $ case op of
                   "start"  ->  killChilds >> stream
                   "cancel" ->  killChilds >> stop

         teleport          -- back to the browser again
         local $ render $ rawHtml $ h1 r
   where
   -- generates a sequence of numbers
   stream= do
     counter <- liftIO $ newIORef (0 :: Int)
     waitEvents $ do
          n <- atomicModifyIORef counter $ \r -> (r +1,r)
          threadDelay 1000000
          putStr "generating: " >> print n
          return  n

startOrCancel :: Cloud String
startOrCancel= local $ render $   (inputSubmit "start"  `fire` OnClick)
                              <|> (inputSubmit "cancel" `fire` OnClick)
                              <++ br


