

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Transient.Base
import           Transient.Backtrack
import           Transient.Indeterminism
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.State
import           Data.Monoid
import           System.IO.Unsafe

import           Network.HTTP

import           Network
import           System.IO

-- show

solveConstraint=  do
      x <- choose  [1,2,3]
      y <- choose  [4,5,6]

      guard $ x * y == 8

      return (x,y)

pythags = do
  x <- choose [1..50]
  y <- choose([1..x] :: [Int])
  z <- choose[1..round $ sqrt(fromIntegral $ 2*x*x)]

  guard (x*x+y*y==z*z)

  return (x, y,z)

example1= do
    option "ex1" "example 1"
    r <- threads 4 solveConstraint
    liftIO $ print r

example2= do
    option "pyt" "pythagoras"
    r<- threads 4 pythags
    liftIO $ print r

collectSample= threads 4 $ do
    option "coll" "collect sample: return results in a list"
    r <- collect 9 $ do
      x <- choose  [1,2,3]
      y <- choose  [4,5,6]
      return (x,y)

    liftIO $ print r

threadSample= do
     option "th" "threads sample"
     liftIO $ print "number of threads? (< 10)"

     n <- input  ( < 10)

     threads n $ do
        x <- choose  [1,2,3]
        y <- choose  [4,5,6]
        th <- liftIO $ myThreadId
        liftIO $ print (x,y,th)

nonDeterminsm= do
      option "nondet" "Non determinism exaples"
      example1 <|> example2
               <|> collectSample
               <|> threadSample


main= keep $ do
      oneThread $ option "main" "to return to the main menu"   <|> return ""
      liftIO $ putStrLn "MAIN MENU"


      nonDeterminsm <|> trans <|>
            colors  <|>  app  <|>
            futures <|>  server

-- / show

trans= do
       option "trans" "transaction examples with backtracking for undoing actions"
       transaction <|> transaction2

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
            liftIO  (putStrLn "update other database necesary for the reservation")
                 `onUndo` liftIO (putStrLn "database update undone")



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
       liftIO $ putStrLn "to stop the sequence, write main(enter)"
       counter <- liftIO $ newMVar 0

       r <-  (,) <$>  number  counter 1 <*> number counter 1


       liftIO $ putStrLn $ "result=" ++ show r
       where
       number counter n= waitEvents $ do
          threadDelay $ n * 1000000
          n <- takeMVar counter
          putMVar counter (n+1)
          return n

futures= do
       option "async" "for parallelization of IO actions with applicative and monioidal combinators"
       sum1 <|> sum2

sum1 :: TransientIO ()
sum1= do
       option "sum1" "access to two web pages concurrently and sum the number of words using Applicative"
       liftIO $ print " downloading data..."
       (r,r') <- (,) <$> async  (worker "http://www.haskell.org/")
                     <*> async  (worker "http://www.google.com/")

       liftIO $ putStrLn $ "result="  ++ show  (r + r')

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
      liftIO $ print " downloading data..."
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


