{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where 

import           Base
import           Backtrack
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Monoid
import           System.IO.Unsafe

import           Network.HTTP

import           Network
import           System.IO
import           Data.IORef
import           Control.Monad

-- show1

events :: Loop -> [a] -> TransientIO a
events typ xs = do
    evs <- liftIO  $ newIORef xs
    r <- parallel typ $ do
           atomicModifyIORef evs $ \es -> do
              if not $ null es 
                then  (tail es, Just $ head es)
                else (es,Nothing)

    case r of
        Nothing -> stop
        Just r -> return r



choose= events  Loop 

pythags = do
  x <- choose[1..3]
  y <- choose[1..3]
  z <- choose[1..3]
  -- guard (x+y==4)
  liftIO $ print (x, y,z)


solve=do
    option "solve" "indeterminism example"
    pythags 

main= do
    runTransient $ do
  --    r <- (,) <$> parallel Loop (return 1) <*> parallel Loop (return 2)
  --    liftIO $ print r
  --    stop

  

 

      option "main" "to return to the main menu"  <|> return ""
      liftIO $ putStrLn "MAIN MENU"

      --transaction <|> transaction2 <|> 
      colors <|>  app  <|> sum1 <|> sum2 <|> server <|> menu <|> solve

    stay

{-
transaction=   do
       option "back" "backtracking test"
       productNavigation
       reserve
       payment

transaction2= do
       option "back2" "backtracking test 2"
       productNavigation
       reserveAndSendMsg
       payment


       liftIO $ print "done!"

  
productNavigation = liftIO $ putStrLn "product navigation" 

reserve= liftIO (putStrLn "product reserved,added to cart") 
                 `onUndo` liftIO (putStrLn "product un-reserved") 

payment = do
           liftIO $ putStrLn "Payment failed"
           undo

reserveAndSendMsg= do
            reserve
            liftIO $ print "MIDDLE"
            liftIO  (putStrLn "update other database necesary for the reservation")
                 `onUndo` liftIO (putStrLn "database update undone")

-}

colors :: TransientIO ()
colors= do
       option "colors" "choose between three colors"
       r <-  color 1  "red"  <|> color 2 "green" <|> color 3 "blue"
       liftIO $ print r
       where
       color :: Int -> String -> TransientIO String
       color n str= option (show n) str >> return  str

app :: TransientIO ()
app= do
       option "app" "applicative expression that return a counter in 2-tuples every second"
       r <-  (,) <$>  number  <*> number
       liftIO $ putStrLn $ "result=" ++ show r
       where
       number= waitEvents $ do
          threadDelay 1000000
          n <- takeMVar counter
          putMVar counter (n+1)
          return  n

       counter=unsafePerformIO $ newMVar (0 :: Int)

sum1 :: TransientIO ()
sum1= do
       option "sum1" "access to two web pages concurrently and sum the number of words using Applicative"
       (r,r') <- (,) <$> async  (worker "http://www.haskell.org/")
                     <*> async  (worker "http://www.google.com/")

       liftIO $ putStrLn $ "result="  ++ show (r + r')

getURL= simpleHTTP . getRequest

worker :: String -> IO Int
worker url=do
      r <- getURL url
      body <- getResponseBody r
      putStrLn $ "number of words in " ++ url ++" is: " ++ show(length (words body))
      return . length . words $ body

sum2 :: TransientIO ()
sum2= do
       option "sum2" "access to N web pages concurrenty and sum the number of words using map-fold"
       rs <- foldl (<>) (return 0) $ map (async . worker)
                  [ "http://www.haskell.org/"
                  , "http://www.google.com/"]

       liftIO $ putStrLn $ "result="  ++ show rs

instance Monoid Int where
      mappend= (+)
      mempty= 0

server :: TransientIO ()
server=  do
       option "server" "A web server in the port 8080"
       liftIO $ print "Server Stated"
       sock <-  liftIO $  listenOn $ PortNumber 8080
       (h,_,_) <- spawn $ accept sock
       liftIO $ do
           hPutStr h msg
           putStrLn "new request"
           hFlush h
           hClose h
         `catch` (\(e::SomeException) -> sClose sock)

msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"


menu :: TransientIO ()
menu=  do
     option "menu"  "a submenu with two options"
     colors  <|> sum2 

-- / show




