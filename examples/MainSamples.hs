

{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Data.Typeable
import           Transient.Base
import           Transient.Backtrack
import           Transient.Indeterminism
import           Transient.Logged
import           Transient.Move
import           Transient.EVars
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.State
import           Data.Monoid
import           System.IO.Unsafe
import           System.Directory
import           System.FilePath
import           Network.HTTP
import           Network
import           System.IO
import           Data.IORef

import Data.List hiding (find,map, group)


main= keep $ do
      oneThread $ option "main" "to kill previous spawned processes and return to the main menu"   <|> return ""
      liftIO $ putStrLn "MAIN MENU"

      nonDeterminsm <|> trans <|>
             colors <|> app   <|>
            futures <|> server <|>
            distrib <|> pubSub

solveConstraint=  do
      x <- choose  [1,2,3]
      y <- choose  [4,5,6]

      guard $ x * y == 8

      return (x,y)

pythags = freeThreads $ do
  x <- choose [1..50]
  y <- choose ([1..x] :: [Int])
  z <- choose [1..round $ sqrt(fromIntegral $ 2*x*x)]

  guard (x*x+y*y==z*z)
  th <- liftIO  myThreadId
  return (x, y, z, th)

example1= do
    option "ex1" "example 1"
    r <- threads 4 solveConstraint
    liftIO $ print r

example2= do
    option "pyt" "pythagoras"
    r<- threads 2 pythags
    liftIO $ print r

collectSample= threads 4 $ do
    option "coll" "collect sample: return results in a list"
    r <- collect 0 $ do
      x <- choose  [1,2,3]
      y <- choose  [4,5,6]
      th <- liftIO $ threadDelay 1000 >> myThreadId

      return (x,y,th)

    liftIO $ print r

threadSample= do
     option "th" "threads sample"
     liftIO $ print "number of threads? (< 10)"

     n <- input  ( < 10)

     threads n $ do
        x <- choose  [1,2,3]
        y <- choose  [4,5,6]
        th <- liftIO myThreadId
        liftIO $ print (x,y,th)

nonDeterminsm= do
      option "nondet" "Non determinism examples"
      example1 <|> example2
               <|> collectSample
               <|> threadSample
               <|> fileSearch



find' :: String -> FilePath -> TransientIO FilePath
find' s d = do
  fs <- liftIO $ getDirectoryContents d
       `catch` \(e:: SomeException) -> return []       -- 1
  let fs' = sort $ filter (`notElem` [".",".."]) fs    -- 2
  if any (== s) fs'                                    -- 3
     then do
       liftIO $ print $ d </> s
       return $ d</> s
     else do
       f <- choose fs'                                 -- 4
       let d' = d </> f                                -- 6
       isdir <- liftIO $ doesDirectoryExist d'         -- 7
       if isdir then find' s d'                        -- 8
                else stop


------------------

fileSearch=   do
    option "file" "example of file search"
    r<- threads 3 $ collect 10 $ find' "MainSamples.hs"  "."
    liftIO $ putStrLn $ "SOLUTION= "++ show  r
--    exit







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

       r <-  (,) <$>  number  <*> snumber

       liftIO $ putStrLn $ "result=" ++ show r
       where
       number= do
          counter <- liftIO $ newMVar (0 :: Int)
          waitEvents $ do
              threadDelay  1000000
              n <- takeMVar counter
              putMVar counter (n+1)
              return n

       snumber = number >>= return . show

futures= do
       option "async" "for parallelization of IO actions with applicative and monoidal combinators"
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

       (h,_,_) <- spawn $ accept sock `catch` (\(e::SomeException) -> sClose sock >> throw e)
       liftIO $ do
           hPutStr h msg
           putStrLn "new request"
           hFlush h
           hClose h
         `catch` (\(e::SomeException) -> sClose sock)

msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"


-- distributed computing

distrib = do
    option "distr" "examples of distributed computing"
    let port1 =  2000

    let node =createNode host port1
    addNodes [node]
    distributed $ do
      listen  node <|> return ()-- conn port1 port1 <|> conn port2 port1

      examples' node
      where
      host= "localhost"


examples' node= do
   local $ option "maind"  "to see this menu" <|> return ""
   r <-local    $ option "move" "move to another node"
               <|> option "call" "call a function in another node"
               <|> option "chat" "chat"
               <|> option "netev" "events propagating trough the network"
   case r of
       "call"  -> callExample node
       "move"  -> moveExample node
       "chat"  -> chat
       "netev" -> networkEvents node


callExample node= do
   local $ putStrLnhp  node "asking for the remote data"
   s <- callTo node $ cexec $ liftIO $ do
                       putStrLnhp  node "remote callTo request"
                       readIORef environ


   cexec . liftIO $ putStrLn $ "resp=" ++ show s

{-# NOINLINE environ #-}
environ= unsafePerformIO $ newIORef "Not Changed"

moveExample node= do
   cexec $ putStrLnhp  node "enter a string. It will be inserted in the other node by a migrating program"
   name <- local $ input (const True)
   beamTo node
   cexec $ liftIO $ putStrLnhp  node "moved!"
   cexec $ liftIO $ putStrLnhp  node $ "inserting "++ name ++" as new data in this node"
   cexec $ liftIO $ writeIORef environ name
   return()


chat ::  Cloud ()
chat  = do
    name  <- local $ do liftIO $ putStrLn "Name?" ; input (const True)
    text <- local $  waitEvents  $ putStr ">" >> hFlush stdout >> getLine' (const True)
    let line= name ++": "++ text
    clustered $   cexec $ liftIO $ putStrLn line


networkEvents node= do
     local $  do
       putStrLnhp  node "callTo is not  a simple remote call. it stablish a connection"
       putStrLnhp  node "between transient processes in different nodes"
       putStrLnhp  node "in this example, events are piped back from a remote node to the local node"

     r <- callTo node $ do
                         local $ option "fire"  "fire event"
                         return "event fired"
     cexec $ putStrLnhp node $ r ++ " in remote node"

putStrLnhp p msg= liftIO $ putStr (show p) >> putStr " ->" >> putStrLn msg


pubSub=  do
  option "pubs" "an example of publish-subscribe using Event Vars (EVars)"
  v  <- newEVar  :: TransIO (EVar String)
  v' <- newEVar

  subscribe v v' <|> publish v v'
  where

  publish v v'= do
    liftIO $ putStrLn "Enter a message to publish"
    msg <-  input(const True)

    writeEVar v msg
    liftIO $ putStrLn "after writing first EVar\n"

    liftIO $ putStrLn "Enter a second message to publish"
    msg <-  input(const True)

    writeEVar v'  msg
    liftIO $ putStrLn "after writing second EVar\n"
    publish v v'


  subscribe :: EVar String -> EVar String -> TransIO ()
  subscribe v v'= do
       r <- (,) <$> proc1 v  <*>  proc2 v'
       liftIO $ do
             putStr "applicative result= "
             print r

  subscribe2 ::  EVar String -> EVar String -> TransIO ()
  subscribe2 v v'= do
        x <-  readEVar v
        liftIO $ print "read evar1 in monad"
        y <- readEVar v'
        liftIO $ do
                 putStr "monadic result"
                 print (x,y)

  proc1 v=  do
    msg <- readEVar v
    liftIO $ putStrLn $  "proc1 readed var: " ++ show msg
    return msg

  proc2 v= do
    msg <- readEVar v
    liftIO $ putStrLn $ "proc2 readed var: " ++ show msg
    return msg






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
