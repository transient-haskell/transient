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


-- Show the composability of transient web aplications
-- with three examples composed together, each one is a widget that execute
-- code in the browser AND the server.

main = simpleWebApp 2020 $  demo <|> demo2  <|>  counters


demo= do
   name <- local . render $ do
       rawHtml $ do
          hr
          p "this snippet captures the essence of this demonstration"
          p $ span "it's a blend of server and browser code in a "
               >> (span $ b "composable") >> span " piece"

          div ! id (fs "fibs") $ i "Fibonacci numbers should appear here"

   local . render $ wlink () (p " stream fibonacci numbers")

   -- stream fibonancci

   r <-  atServer $ do
               let fibs= 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]  --fibonacci numb. definition

               r <- local  . threads 1 . choose $ take 10 fibs
               lliftIO $ print r
               lliftIO $ threadDelay 1000000
               return r

   local . render . at (fs "#fibs") Append $ rawHtml $  (h2 r)



demo2= do
   name <- local . render $ do
       rawHtml $ do
          hr
          br;br
          p "In this example you enter your name and the server will salute you"
          br

       inputString (Just "Your name") `fire` OnKeyUp <++ br

   r <- atServer $ lliftIO $ print (name ++ " calling") >> return ("Hi " ++ name)

   local . render . rawHtml $ do
            p " returned"
            h2 r



fs= toJSString




-- To demonstrate wormhole, teleport, widgets, interactive streaming
-- and composability in a web application.
--
-- This is one of the most complicated interactions: how to control a stream in the server
-- by means of a web interface without loosing composability.
--
-- in this example, events flow from the server to the browser (a counter) and back from
-- the browser to the server (initiating and cancelling the counters)


counters= do
   local . render . rawHtml $ do
          hr
          p "To demonstrate wormhole, teleport, widgets, interactive streaming"
          p "and composability in a web application."
          br
          p "This is one of the most complicated interactions: how to control a stream in the server"
          p "by means of a web interface without loosing composability."
          br
          p "in this example, events flow from the server to the browser (a counter) and back from"
          p "the browser to the server (initiating and cancelling the counters)"

   server <- local $ getSData <|> error "no server???"
   wormhole  server $ counter <|> counter
   where
   counter  =  do

         op <-  startOrCancel
         teleport          -- translates the computation to the server
         r <- local $ case op of
                   "start"  ->  killChilds >> stream
                   "cancel" ->  killChilds >> stop

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

startOrCancel :: Cloud String
startOrCancel= local $ render $   (inputSubmit "start"  `fire` OnClick)
                              <|> (inputSubmit "cancel" `fire` OnClick)
                              <++ br


