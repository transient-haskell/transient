module Main where

import Transient.Move
import Transient.Base
import Network
import Control.Applicative
import Data.IORef
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Data.Monoid
import System.IO


main= do
  args <- getArgs
  if length args < 3 then do
     print "the program need three parameters:  localPort  remoteHost RemotePort"
     return ()
    else do

      let localPort= PortNumber . fromInteger . read $ args !! 0
          remoteHost= args !! 1
          remotePort= PortNumber . fromInteger . read $ args !! 2

      beamInit localPort $ do

       r <-oneThread $ step $   option "move" "move to another node"
                <|> option "call" "call a function in another node"
                <|> option "chat" "chat"

       case r of
         "call" -> callExample remoteHost remotePort
         "move" -> moveExample remoteHost remotePort
         "chat" -> chat [(remoteHost,remotePort)]


callExample host port= do
   step $ liftIO $ putStrLn "asking for the remote data"
   s <- callTo host port $ liftIO $ do
                       putStrLn "remote callTo request"
                       readIORef environ


   liftIO $ putStrLn $ "resp=" ++ show s


environ= unsafePerformIO $ newIORef "Not Changed"

moveExample host port= do
   step $ liftIO $ putStrLn "enter a string, that will be inserted in the other node by a migrating program"
   name <- step $ input (const True)
   beamTo host port
   liftIO $ print "moved!"
   liftIO $ print $ "inserting "++ name ++" as new data in this node"
   liftIO $ writeIORef environ name
   return()




chat :: [(HostName, PortID)] -> Transient StateIO ()
chat nodes = do
    name  <- step $ do liftIO $ putStrLn "Name?" ; input (const True)
    text <- step $  waitEvents  $ putStr ">" >>hFlush stdout >> getLine' (const True)
    let line= name ++": "++ text
    foldl (<>) mempty $ map (\(h,n) -> callTo h  n  $ liftIO $ putStrLn line) nodes



