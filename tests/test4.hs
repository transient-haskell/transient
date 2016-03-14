{-# LANGUAGE OverloadedStrings #-}
import Transient.Base
import Transient.Move
import Transient.Logged
import Data.IORef
import Control.Monad.IO.Class
import Data.Typeable
import Control.Applicative
import Data.CaseInsensitive(mk)
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.Monoid
import System.IO.Unsafe

data ParseContext a = ParseContext (IO  a) a deriving Typeable

main= keep' $ do

      let port1 =  2000

      let node =createNode host port1
      addNodes [node]
      listen  node <|> return ()-- conn port1 port1 <|> conn port2 port1

      moveExample node
      where
      host= "localhost"

putStrLnhp p msg= liftIO $ putStr (show p) >> putStr " ->" >> putStrLn msg

{-# NOINLINE environ #-}
environ= unsafePerformIO $ newIORef "Not Changed"


moveExample node= do
   putStrLnhp  node "enter a string. It will be inserted in the other node by a migrating program"
   let name = "aaaaa"
   beamTo node
   putStrLnhp  node "moved!"
   putStrLnhp  node $ "inserting "++ name ++" as new data in this node"
   liftIO $ writeIORef environ name
   return()

--evs= unsafePerformIO $ newIORef ["LOG a b","","notparameter"]
--
--
----giveData= atomicModifyIORef' evs $ \es -> if null es then (mempty,mempty) else let tes= tail es in (tes,head es)
--giveData= do
--   r <- readIORef rend
--
--   liftIO $ putStr "rend=">> print r
--   if r then return "" else do
--    r<- atomicModifyIORef' evs $ \es -> let tes= tail es in (tes,head es)   !!> "GETLINE"
--    liftIO $ print r
--    if r=="\r" || r == "" then do
--       writeIORef rend True
--       return ""
--       else return r
--   where
--   rend= unsafePerformIO $ newIORef False
--
--
--
--receiveHTTPHead  = do
--    setSData $ ParseContext (giveData ) ("":: BC.ByteString)
--    (method, uri, vers) <- (,,) <$> getMethod <*> getUri <*> getVers
--    headers <- many $ (,) <$> (mk <$> getParam) <*> getParamValue
--    return (method, uri, headers)
--
--getMethod= getString
--getUri= getString
--getVers= getString
--getParam= do
--      dropSpaces
--      r <- tTakeWhile (\x -> x /= ':' && x /= '\r')
--      if BC.null r || r=="\r"  then  stop  else  dropChar >> return r
--
--getParamValue= dropSpaces >> tTakeWhile  (/= '\r')  !!> "getParamValue"
--
--dropSpaces= parse $ \str ->((),BC.dropWhile isSpace str)
--
--dropChar= parse  $ \r -> ((), BC.tail r)
--
--getString= do
--    dropSpaces
--    tTakeWhile (not . isSpace)
--
--tTakeWhile :: (Char -> Bool) -> TransIO BC.ByteString
--tTakeWhile cond= parse (BC.span cond)
--
--parse :: (Typeable a, Eq a, Show a, Monoid a,Monoid b) => (a -> (b,a)) -> TransIO b
--parse split= do
--    ParseContext rh str <- getSData <|> error "parse: ParseContext not found"
--    if  str == mempty then do
--          str3 <- liftIO  rh
--
--          setSData $ ParseContext rh str3  !!> ("str3=" ++ show str3)
--
--          if str3== mempty then stop !!> "stop"  else  parse split
--       else do
--          cont <- do
--             let (ret,str3) = split str
--             setSData $ ParseContext rh str3
--             if  str3 == mempty
--                then  return $ Left ret
--                else  return $ Right ret
--          case cont of
--            Left r -> do
--                r' <- (<>) <$> return r  <*> (parse split <|> return mempty)
--
--                return r'
--            Right r ->   return r
--

