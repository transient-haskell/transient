{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable #-}
{- TODO: test it. class Distributable, Shuffle (Distributable for DDS)
class Distributable a where
   distibute a -> [b]
 instance distributable (DDS (key,a) where
   distribute= shuffle the data by key among the nodes
 -}

module Transient.DDS(distribute, cmap, reduce) where
import Transient.Base hiding (stop)
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

import Data.ByteString.Lazy.Char8 (pack,unpack)
import Control.Monad.STM
import qualified Data.Map as M
import Control.Arrow (second)
import qualified Data.Vector as V


data DDS a= Loggable a => DDS  (Cloud (PartRef a))
data PartRef a= Ref Node Path Save deriving (Typeable, Read, Show)
data Partition a=  Part Node Path Save a deriving (Typeable,Read,Show)
type Save= Bool

instance Indexable (Partition a) where
    key (Part _ string b _)= keyp string b


keyp s True= "PartP@"++s
keyp s False="PartT@"++s

instance Loggable a => IResource (Partition a) where
    keyResource= key
    readResourceByKey k= if k!! 5 /= 'P' then return Nothing
        else defaultReadByKey k >>= return . fmap ( read . unpack)
    writeResource (s@(Part _ _ save _))=
          unless (not save) $ defaultWrite (defPath s ++ key s) (pack $ show s)




type Path=String



cmap :: Loggable b => (a -> b) -> DDS a -> DDS b
cmap f (DDS mx)= DDS $  do
        refs <-  mx
        process refs

  where
--  process ::  Partition a -> Cloud [Partition b]
  process  (ref@(Ref node path sav))= runAt node $  do
              xs <- getPartitionData ref -- mx
              ref <- generateRef node $ f   xs
              return ref

unlift :: Cloud a -> TransIO a
unlift (Cloud mx)= mx



data ReduceChunk a= EndReduce | Reduce a deriving (Typeable, Read, Show)

reduce ::  (Enum k,Ord k, Loggable k, Loggable a,Loggable b)
             => (V.Vector a -> b) -> DDS (M.Map k (V.Vector a)) ->Cloud (M.Map k b)
reduce red  (DDS mx)= loggedc $ do
     ref <- mx
     dat <- getPartitionData ref

     let lengthdat= M.size dat
     count <- onAll . liftIO $ newMVar 0
     parallelize shuffle (M.assocs dat)

     count <- onAll . liftIO $ modifyMVar count (\c ->return(c +1,c+1))
     when (count== lengthdat) . clustered $ putMailBox (EndReduce `asTypeOf` paramOf (DDS mx)) --finish
     empty
    <|> do
     maps <-local . collect 0 $ unlift (clustered reducers)
     return $ mconcat maps

     where
     paramOf  :: DDS (M.Map k (V.Vector a)) -> ReduceChunk(M.Map k (V.Vector a))
     paramOf = undefined -- type level

--     groupByKey :: Ord k2 => [(k2, v2)] -> M.Map k2 [v2]
--     groupByKey = M.fromListWith (++) . map (second return)

     reducers = do
       reduceResults <- onAll . liftIO $ newMVar M.empty

       minput <- getMailBox

       case minput of
         Reduce (k,input) -> do
              onAll . liftIO $ withMVar reduceResults $ \r -> return (M.insert k (red input) r)
              empty
         EndReduce    -> onAll . liftIO $ readMVar reduceResults

     shuffle :: (Loggable xs, Loggable k, Enum k) => (k,xs) -> Cloud ()
     shuffle (k,ds) = do
           nodes <- onAll getNodes
           let i=  fromEnum k `rem` length nodes
           beamTo  (nodes !! i)
           putMailBox  (k,ds)

parallelize f xs= loggedc $ foldl (<|>) empty $ map  f xs







getPartitionData :: Loggable a => PartRef a   -> Cloud  a
getPartitionData (Ref node path save)  = do
    Just (Part _ _ _ xs) <- local . liftIO $ atomically
                                       $ readDBRef
                                       $ getDBRef
                                       $ keyp path save
 --                                                   `asTypeOf` getPartitionType mx)
    return xs
    where
    getPartitionType :: Cloud (PartRef a)-> Partition a
    getPartitionType = undefined -- type level only

-- en caso de fallo de Node, se lanza un clustered en busca del path
--   si solo uno lo tiene, se copia a otro
--   se pone ese nodo de referencia en Part
runAtP :: Loggable a => Node  -> (Path -> IO a) -> Path -> Cloud a
runAtP node f uuid= do
   r <- streamFrom node $ onAll . liftIO $ (SLast <$> f uuid) `catch` sendAnyError
   case r of
     SLast r -> return r
     SError e -> do
         nodes <-  mclustered $ search uuid
         when(length nodes < 1) $ asyncDuplicate node uuid
         runAtP ( head nodes) f uuid

search uuid= error $ "chunk failover not yet defined. Lookin for: "++ uuid

asyncDuplicate node uuid= do
    forkTo node
    nodes <- onAll getNodes
    let node'= head $ nodes \\ [node]
    content <- onAll . liftIO $ readFile uuid
    runAt node' $ local $ liftIO $ writeFile uuid content

sendAnyError :: SomeException -> IO (StreamData a)
sendAnyError e= return $ SError  e


distribute :: Loggable a => V.Vector a -> DDS (V.Vector a)
distribute = DDS  .  distribute'

distribute' xs= loggedc $  do
   nodes <- local getNodes
   let size= length xs `div` length nodes
       xss = split size xs
   distribute'' xss nodes
   where
   split n xs | V.null xs = []
   split n xs=
      let (h,t)= V.splitAt n xs
      in h : split n t

distribute'' :: Loggable a => [V.Vector a] -> [Node] -> Cloud (PartRef (V.Vector a))
distribute'' xss nodes =
   parallelize  move $ zip nodes xss   -- !> show xss
   where
   move (node, xs)=  runAt node $ do
                        par <- generateRef node xs
                        return  par




textFile name= DDS $ loggedc $ do
   lines <- onAll . liftIO $ liftM  lines (readFile name)
   distribute' $ V.fromList lines




generateRef :: Loggable a => Node -> a -> Cloud (PartRef a)
generateRef node x= onAll . liftIO $ do
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
     Integer -> IO (StreamData a) -> DDS (V.Vector a)
streamDDS time io= DDS $ do
     xs <- local . groupByTime time $ do
               r <- parallel io
               case r of
                    SDone -> empty
                    SLast x -> return x
                    SMore x -> return x
                    SError e -> error $ show e
     distribute'  $ V.fromList xs




--data CloudArray a= Cloud [a]
--
--instance functor  CloudArray where
--   fmap f mx= do
--        xs <- mx
--        xss <- partition xs
--        rss <- clustered f xss
--        return $ concat rss


