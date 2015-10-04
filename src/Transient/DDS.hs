{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable #-}

module Transient.DDS where
import Transient.Base
import Transient.Move
import Transient.Logged

import Control.Applicative
import System.Random
import Control.Monad.IO.Class

import System.IO
import Control.Monad
import Data.Monoid

import Data.Typeable
import Data.List
import Control.Exception


data DDS a= Loggable a => DDS (TransIO [Elem [a]])
data Elem a= Ref Node Path deriving (Typeable,Read,Show)

instance Loggable a => Monoid (DDS a) where
   mempty= DDS mempty
   mappend (DDS ta) (DDS tb)= DDS $ ta <> tb



type Path=String

cmap :: Loggable b => (a -> b) -> DDS a -> DDS b
cmap f (DDS mx)= DDS $ logged $ do
        xs <- logged mx
        foldl (<>) mempty $  map (process (map f)) xs

  where
 -- process :: (a -> b) -> Elem a -> TransIO[Elem b]
--  process f (Local xs)= return [Local $ f xs]
  process f (Ref node path)= runAt node $ f1 f node path

--  f1 :: Loggable b => ([a] -> [b]) -> Node -> String -> TransIO [Elem b]
  f1  f node path=liftIO $ do
        str <- readFile path
        let cont= read str                        !> ("READ: " ++ str)
        temp <- generateRef $ show $ f cont
        return [Ref node temp]

reduce ::  (a -> a -> a)  -> DDS a ->TransientIO a
reduce f   (DDS mx)= logged $ do
     xs <- logged mx
     logged $ fold (\ x y -> f <$> x <*> y) $  map process  xs

     where
--     process f  (Local xs)= return  $ foldl f  xs
     process (Ref node path)= runAt node $ f1 path

     f1 path=liftIO $ do
         str <- readFile path
         let cont= read str                        !> ("REDUCE : "++ str)
         return  $ fold f  cont
     fold _ []= error "reduce: empty data set"
     fold f xs= foldl f (head xs)(tail xs)

-- en caso de fallo de Node, se lanza un clustered en busca del path
--   si solo uno lo tiene, se copia a otro
--   se pone ese nodo de referencia en Ref
--runAtP :: Loggable a => Node  -> (Path -> IO a) -> Path -> TransIO a
--runAtP node f uuid= do
--   r <- streamFrom node $ liftIO $ (SLast <$> f uuid) `catch` sendAnyError
--   case r of
--     SLast r -> return r
--     SError e -> do
--         nodes <- collect 0 $ clustered $ search uuid
--         when(length nodes < 1) $ asyncDuplicate node uuid
--         runAtP (nodes !! 0) f uuid
--   where
--   search uuid= error $ "chunk failover not yet defined. Lookin for: "++ uuid

asyncDuplicate node uuid= do
    forkTo node
    nodes <- getNodes
    let node'= head $ nodes \\ [node]
    content <- liftIO $ readFile uuid
    runAt node' $ liftIO $ writeFile uuid content

sendAnyError :: SomeException -> IO (StreamData a)
sendAnyError e= return $ SError $ show e


distribute :: Loggable a => [a] -> DDS a
distribute xs= DDS  $ logged $ do
   nodes <- logged $ getNodes
   let size= length xs `div` length nodes
       xss = split size xs
   foldl (<>) mempty $ map (\(node,xs) -> move node xs) (zip nodes  xss)   !> show xss
   where
   move node xs=  runAt node $ liftIO $ do
       ref <- generateRef $ show xs
       return [Ref node ref]

   split n []= []
   split n xs=
      let (h,t)= splitAt n xs
      in h : split n t

generateRef x= do
       temp <- getTempName
       liftIO $ writeFile temp  x                         !> ("write "++x)
       return temp

getTempName :: IO String
getTempName=  ("DDS/" ++) <$> replicateM  5 (randomRIO ('a','z'))
