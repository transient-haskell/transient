{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable #-}

module Transient.DDS where
import Transient.Base
import Transient.Move
import Transient.Logged
import Transient.Indeterminism

import Control.Applicative
import System.Random
import Control.Monad.IO.Class

import System.IO
import Control.Monad
import Data.Monoid

import Data.Typeable
import Data.List hiding (delete)
import Control.Exception
import Control.Concurrent
import Data.Time.Clock

import Data.TCache
import Data.TCache.Defs
--import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Control.Monad.STM


data DDS a= Loggable a => DDS  (TransIO [PartRef a])
data PartRef a= Ref Node Path Save deriving (Typeable, Read, Show)
data Partition a=  Part Node Path Save [a] deriving (Typeable,Read,Show)
type Save= Bool

instance Indexable (Partition a) where
    key (Part _ string True _)= "PartP@"++string
    key (Part _ string False _)= "PartT@"++string

instance Loggable a => IResource (Partition a) where
    keyResource= key
    readResourceByKey k= if k!! 5 /= 'P' then return Nothing
        else defaultReadByKey k >>= return . fmap ( read . unpack)
    writeResource (s@(Part _ _ save _))= if not save then return ()
        else defaultWrite (defPath s ++ key s) (pack$ show s)
--instance Loggable a => Serializable (Partition a) where
--    serialize = pack . show
--    deserialize = read . unpack
--    setPersist (Part _ _ persist _) =
--      Just Persist
--        {readByKey= if persist then defaultReadByKey else const $ return Nothing
--        ,write    =  if persist then defaultWrite else const $  const $ return ()
--        ,delete   = defaultDelete}


instance Loggable a => Monoid (DDS a) where
   mempty= DDS mempty
   mappend (DDS ta) (DDS tb)= DDS $ ta <> tb



type Path=String

cmap :: Loggable b => (a -> b) -> DDS a -> DDS b
cmap f (DDS mx)= DDS $ logged $ do
        refs <- logged mx
        foldl (<>) mempty $  map process refs

  where
--  process ::  Partition a -> TransIO [Partition b]
  process  (ref@(Ref node path sav))= runAt node $  do
              xs <- getPartitionData ref mx
              ref <- generateRef node $ map f xs
              return [ref]





reduce' :: (Loggable b, Monoid b) => ([a] -> b) -> DDS a -> TransientIO b
reduce' f = reduce f mappend mempty

reduce ::  Loggable b => ([a] -> b) -> (b -> b -> b)-> b -> DDS a ->TransientIO b
reduce f f2 seed (DDS mx)= logged $ do
     refs <- logged mx
     logged $ foldl (\ x y -> f2 <$> x <*> y)(return seed) $  map process refs

     where

--     process :: Partition a -> TransIO b
     process (ref@(Ref node _ _))= runAt node $ do
               xs <- getPartitionData ref mx
               return $ f xs

getPartitionData (Ref node path save) mx= do
    Just (Part _ _ _ xs) <- liftIO $ atomically
                                       $ readDBRef
                                       $ getDBRef
                                       $ keyResource((Part node path save undefined)
                                                    `asTypeOf` getPartitionType mx)
    return xs
    where
    getPartitionType :: TransIO [PartRef a]-> Partition a
    getPartitionType = undefined -- type level only

-- en caso de fallo de Node, se lanza un clustered en busca del path
--   si solo uno lo tiene, se copia a otro
--   se pone ese nodo de referencia en Part
runAtP :: Loggable a => Node  -> (Path -> IO a) -> Path -> TransIO a
runAtP node f uuid= do
   r <- streamFrom node $ liftIO $ (SLast <$> f uuid) `catch` sendAnyError
   case r of
     SLast r -> return r
     SError e -> do
         nodes <-  mclustered $ search uuid
         when(length nodes < 1) $ asyncDuplicate node uuid
         runAtP ( head nodes) f uuid

search uuid= error $ "chunk failover not yet defined. Lookin for: "++ uuid

asyncDuplicate node uuid= do
    forkTo node
    nodes <- getNodes
    let node'= head $ nodes \\ [node]
    content <- liftIO $ readFile uuid
    runAt node' $ liftIO $ writeFile uuid content

sendAnyError :: SomeException -> IO (StreamData a)
sendAnyError e= return $ SError $ show e


distribute :: Loggable a => [a] -> DDS a
distribute = DDS  . logged . distribute'

distribute' xs=  do
   nodes <- logged getNodes
   let size= length xs `div` length nodes
       xss = split size xs
   distribute'' xss nodes
   where
   split n []= []
   split n xs=
      let (h,t)= splitAt n xs
      in h : split n t

distribute'' :: Loggable a => [[a]] -> [Node] -> TransIO[PartRef a]
distribute'' xss nodes =
   foldl (<>) mempty $ zipWith move nodes xss   !> show xss
   where
   move node xs=  runAt node $ do
                        par <- generateRef node xs
                        return  [par]




textFile name= DDS $ logged $ do
   lines <- liftIO $ liftM lines (readFile name)
   distribute' lines

--getId :: DDS a -> TransIO String
--getId (DDS mx)= do
--     ids <- mx
--     let ids' = map (\(Part _  path _ _) -> path) ids
--     return $ "DDS@"++ intercalate ":" ids'


--fromId :: String -> DDS a
--fromId ('D':'D':'S':'@':id)= do
--   let ids= wordsBy (==':') id
--   nodes <- clustered' $ mapM readDBRef ids
--   return
--
--   where
--   wordsBy :: (a -> Bool) -> [a] -> [[a]]
--   wordsBy f s = case dropWhile f s of
--        [] -> []
--        x:xs -> (x:w) : wordsBy f (drop1 z)
--            where (w,z) = break f xs


generateRef :: Loggable a => Node -> [a] -> TransIO (PartRef a)
generateRef node x= liftIO $ do
       temp <- getTempName
       let reg=  Part node temp False  x
       atomically $ newDBRef reg
       return $ getRef reg

getRef (Part n t s x)= Ref n t s

getTempName :: IO String
getTempName=  ("DDS/" ++) <$> replicateM  5 (randomRIO ('a','z'))


-------------- Distributed  Datasource Streams ---------
-- | produce a stream of DDS's that can be map-reduced. Similar to spark streams.
-- each interval of time,a new DDS is produced.
streamDDS
  :: (Typeable a, Show a, Read a) =>
     Integer -> IO (StreamData a) -> DDS a
streamDDS time io= DDS $ do
     xs <- groupByTime time $ do
               r <- parallel io
               case r of
                    SDone -> stop
                    SLast x -> return x
                    SMore x -> return x
                    SError e -> error e
     distribute'  xs







