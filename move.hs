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
import Control.Monad
import Data.Maybe


import Control.Concurrent.STM


main1= runTransient $ do
    let log= [Exec,Step $ toIDyn (Just ())]
    setSData $ Log True log log
    r <- call$ do
         call $ liftIO $ print "hi"
--         Log rec _ log <- getSData
--         liftIO$ print log
         call $ liftIO $ print "how are you"
         return "ret"
    liftIO $ print r
--    Log rec _ log <- getSData
--    liftIO$ print log
  where
  call mx= step $ mx

main= do
  args <- getArgs
  if length args < 2 then do
     print "the program need two parameters:  localHost localPort  remoteHost RemotePort"
     return ()
    else do

      let localHost= args !! 0

          localPort= PortNumber . fromInteger . read $ args !! 1
--      let localHost= "localhost"
--          localPort= PortNumber $ fromIntegral 8080
--          remoteHost= Nothing
--          remotePort= Nothing

      atomically $ writeTVar  myNode (localHost,localPort)

      beamInit localPort $ do
           (remoteHost,remotePort) <- step $ return $ if length args >=4
                then(Just $ args !! 2, Just$ PortNumber . fromInteger . read $ args !! 3)
                else (Nothing,Nothing)

--           h <- liftIO $ connectTo localHost localPort
--           h' <- liftIO $ connectTo localHost localPort
--           liftIO $ print (h,h')
--           stop
--           r <- step $ return  (1 :: Int)
--           step $ liftIO $ print r
--           r <- callTo localHost localPort $ do
--                    step $ return "before hello"
--                    callTo localHost localPort $ liftIO $ print "hello"
--                    return (1 :: Int)
--           liftIO $ print r
--           liftIO $ print "END"
--           stop
           connect  localHost localPort  remoteHost remotePort
           stop
           nodes <- getNodes
           liftIO $ print nodes
           let (remoteHost,remotePort)= head $ tail nodes
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



