module Main where

import Transient.Move
import Transient.Base
import Network
import Control.Applicative
import Data.IORef
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe

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
       r <- step $   option "move" "to move to another node"
                 <|> option "call" "to call a function in another node"

       case r of
         "call" -> callExample remoteHost remotePort
         "move" -> moveExample remoteHost remotePort


callExample host port= do
   step $ liftIO $ putStrLn "asking for the remote data"
--   s1 <- callTo host port $ liftIO $ do
--                       putStrLn "remote callTo request"
--                       readIORef environ
--
--   s2 <- callTo host (PortNumber 8082) $ liftIO $ do
--                       putStrLn "remote callTo request"
--                       readIORef environ
   r <- (,) <$> callTo host port (liftIO $ readIORef environ)
            <*> callTo host (PortNumber 8082) (liftIO $ readIORef environ)
   liftIO $ putStrLn $ "resp=" ++ show r


environ= unsafePerformIO $ newIORef "Not Changed"

moveExample host port= do
   step $ liftIO $ putStrLn "enter a string, that will be inserted in the other node by a migrating program"
   name <- step $ input (const True)
   beamTo host port
   liftIO $ print "moved!"
   liftIO $ print $ "inserting "++ name ++" as new data in this node"
   liftIO $ writeIORef environ name
   return()
