

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Data.Typeable
import           Transient.Base
import           Transient.Backtrack
import           Transient.Indeterminism
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.State
import           Data.Monoid
import           System.IO.Unsafe
import           System.Directory
import           System.FilePath
import           Network.HTTP
import           qualified Data.Map as M
import           Network
import           System.IO
import           Data.IORef
import Text.Parsec hiding (option, (<|>))
import Text.Parsec.Token
import Data.List hiding (find,map, group)
import Control.Concurrent.STM as STM
import GHC.Conc
-- show

solveConstraint=  do
      x <- choose  [1,2,3]
      y <- choose  [4,5,6]

      guard $ x * y == 8

      return (x,y)

pythags = do
  x <- choose [1..50]
  y <- choose ([1..x] :: [Int])
  z <- choose [1..round $ sqrt(fromIntegral $ 2*x*x)]

  guard (x*x+y*y==z*z)

  return (x, y,z)

example1= do
    option "ex1" "example 1"
    r <- threads 4 solveConstraint
    liftIO $ print r

example2= do
    option "pyt" "pythagoras"
    r<- threads 1 pythags
    liftIO $ print r

groupSample= threads 4 $ do
    option "coll" "group sample: return results in a list"
    r <- group 9 $ do
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
               <|> groupSample
               <|> threadSample

find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
       `catch` \(e:: SomeException) -> return []        --1
  let fs' = sort $ filter (`notElem` [".",".."]) fs    -- 2
  if any (== s) fs'                                    -- 3
     then return (Just (d </> s))
     else loop fs'                                     -- 4
 where
  loop [] = return Nothing                             -- 5
  loop (f:fs)  = do
    let d' = d </> f                                   -- 6
    isdir <- doesDirectoryExist d'                     -- 7
    if isdir
       then do r <- find s d'                          -- 8
               case r of
                 Just _  -> return r                   -- 9
                 Nothing -> loop fs                    -- 10
       else loop fs                                    -- 11
       
find' :: String -> FilePath -> TransientIO FilePath
find' s d = do
  fs <- liftIO $ getDirectoryContents d
       `catch` \(e:: SomeException) -> return []       -- 1
  let fs' = sort $ filter (`notElem` [".",".."]) fs    -- 2
  if any (== s) fs'                                    -- 3
     then do
       liftIO $ print $ d </> s
       return $ d</> s
--       found (d </> s :: FilePath)
--       return ()
     else do
       f <- choose fs'                                 -- 4  
       let d' = d </> f                                -- 6
       isdir <- liftIO $ doesDirectoryExist d'         -- 7
       if isdir then find' s d'                        -- 8
                else stop


------------------  

main= keep $  do
    r<-  collect 3 100000 $ threads 1 $  do -- find' "HPPU.log"  "c:\\"
            s <- choose['a'..'c']
            r <- choose[1..4::Int]
            guard  $ s== 'c'
            return (s,r)

    liftIO $ putStrLn $ "SOLUTION= "++ show  r 
--    exit


------------------------

main1 = keep $ do
         r <- group 24 $ threads 10 $ pythags
         liftIO $ print r
         exit

main2= keep $ do
      oneThread $ option "main" "to return to the main menu"   <|> return ""
      liftIO $ putStrLn "MAIN MENU" 

      nonDeterminsm <|> trans <|>
             colors <|> app   <|>
            futures <|> server

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
       color n str= do
         option (show n) str
         liftIO . print $ str ++ " color"
         return  str

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


--main=do
--      r <- getURL "https://www.w3.org/services/html2txt?url=http%3A%2F%2Fwww.searchquotes.com%2Fsearch%2Ftransient%2F"
--      body <- getResponseBody r
--      print $ parse  quote' "" body
--      where
--      quote'= do
--          q <-  between(brackets natural) (brackets natural) string
--
--          if "http" `isPrefixOf` q
--            then quote'
--            else  return q
--

--main    = case (parse numbers "" "11, 2, 43") of
--            Left err  -> print err
--            Right xs  -> print (sum xs)
--
--numbers = commaSep integer
