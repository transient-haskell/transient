{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main where

import Base8
import Control.Monad.State
import System.IO.Unsafe
import Control.Applicative
import Control.Concurrent
import Data.Monoid

import Network.HTTP

import Network
import System.IO

-- show

data Option= Option Int  Int | Number Int deriving Show

instance Monoid Int where
      mappend= (+)
      mempty= 0



main= withSocketsDo $ do
    runTransient $ do
       liftIO $ putStrLn "MAIN MENU"
       r <- async inputLoop  <|> return False
       option "main" "to return to the main menu" <|> return ""
       case r of
         True -> do liftIO $ print "BYE" ; return ()
         False->   colors <|> app  <|> sum1 <|> sum2 <|> server <|> menu

    stay


stay=  takeMVar $ unsafePerformIO $ (newEmptyMVar :: IO (MVar()))

colors= do
       option "colors" "choose between three colors"
       r <-  color 1  "red"   <|> color 2 "green" <|> color 3 "blue"
       liftIO $ do
           putStr "Chosen: "
           print r
       where
       color :: Int -> String -> TransientIO String
       color n str= option n str >> return str

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
      print $ "number of words in " ++ url ++" is: " ++ show(length (words body))
      return . length . words $ body




sum2= do
       option "sum2" "access to two web pages concurrenty and sum the number of words using map-reduce"
       rs <- foldl (<>) (return 0) $ map (async . worker)
                  [ "http://www.haskell.org/", "http://www.google.com/"]

       liftIO $ putStrLn $ "result="  ++ show rs


server=  do
       option "server" "A web server in the port 8080"
       sock <-  liftIO $  listenOn $ PortNumber 8080
       (h,_,_) <- parallel Loop $ accept sock
       liftIO $ do
           hPutStr h msg
           putStrLn "new request"
           hFlush h
           hClose h

msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"


menu=  do
     option "menu"  "a menu with two options"
     r <- option "colors" "colors" <|> option "sum" "sum"
     case r of
        "choose" -> colors
        "sum" -> sum1

-- / show




