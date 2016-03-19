{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable #-}


module Transient.DDS where -- (distribute, cmap, reduce,textFile) where
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
import Data.Maybe

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
import Data.Hashable
import System.IO.Unsafe

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
    readResourceByKey k= if k !! 4 /= 'P' then return Nothing
        else defaultReadByKey k >>= return . fmap ( read . unpack)
    writeResource (s@(Part _ _ save _))=
          unless (not save) $ defaultWrite (defPath s ++ key s) (pack $ show s)


eval :: DDS a -> TransIO (PartRef a)
eval (DDS mx) = runCloud mx


type Path=String

cmap :: (Loggable a,Loggable b,Loggable k,Ord k) => (a -> (k,b)) -> DDS  (V.Vector a) -> DDS (M.Map k(V.Vector b))
cmap f (DDS mx)= DDS $  do
        refs <-  mx
        process refs

  where
--  process ::  Partition a -> Cloud [Partition b]
  process  (ref@(Ref node path sav))= runAt node $ do
              xs <- getPartitionData ref   !> "CMAP"
              generateRef node $ map1 f xs


  map1 :: Ord k => (a -> (k,b)) -> V.Vector a -> M.Map k(V.Vector b)
  map1 f v=  foldl' f1  M.empty v
     where
     f1 map x=
           let (k,r) = f x
           in M.insertWith (V.++) k (V.singleton r) map



instance Show a => Show (MVar a) where
   show mvx= "MVar " ++ show (unsafePerformIO $ readMVar mvx)

instance Read a => Read (MVar a) where
   readsPrec n ('M':'V':'a':'r':' ':s)=
      let [(x,s')]= readsPrec n s
      in [(unsafePerformIO $ newMVar x,s')]

data ReduceChunk a= EndReduce | Reduce a deriving (Typeable, Read, Show)

reduce ::  (Hashable k,Ord k, Loggable k, Loggable a)
             => (a -> a -> a) -> DDS (M.Map k (V.Vector a)) ->Cloud (M.Map k a)
reduce red  (dds@(DDS mx))= do
   box <- createNamedMailBox


   let shuffler=   do
         count <- lliftIO $ newMVar 0   !> "NEWMVAR"
         ref <- mx
         dat <- getPartitionData ref
         let lengthdat= M.size dat

         parallelize shuffle (M.assocs dat)

         cnt <- lliftIO $ modifyMVar count (\c ->return(c +1,c+1))
         when (cnt== lengthdat !> (cnt,lengthdat) ) $ do
              clustered $ putNamedMailBox box (EndReduce `asTypeOf` paramOf dds) !> "ENDREDUCE"
         empty


--     shuffle :: (Loggable v a, Foldable v, Monoid v, Loggable k, Hashable k) => (k,v) -> Cloud ()
       shuffle (k,ds) = do
         nodes <- onAll getNodes  !> k
         let i=  abs $ hash k `rem` length nodes
         runAt  (nodes !! i) $ putNamedMailBox box $ Reduce (k,foldl1 red ds) -- local reduction in the node
                       !> ("shuffle: moving to node, PUTMAILBOX ",i)

       reducer= mclustered reduce    -- a reduce process in each node

--     reduce :: (Ord k)  => Cloud (M.Map k v)
       reduce = local $ do
           reduceResults <- liftIO $ newMVar M.empty  !>  "CReATE reSULtS"
           numberSent <- liftIO $ newMVar 0
           minput <- getNamedMailBox box  -- get the chunk once it arrives to the mailbox

           case minput of
             Reduce (k,inp) -> do
                let input= inp `asTypeOf` atype dds
                return () !> "Reduce"
                liftIO $ modifyMVar_ reduceResults
                       $ \map -> do
                          let maccum =  M.lookup k map
                          return $ M.insert k (case maccum of
                            Just accum -> red input accum
                            Nothing ->  input) map
                empty    !> ("reduce arriving", k)

             EndReduce -> do
                n <- liftIO $ modifyMVar numberSent $ \r -> return (r+1, r+1)
                nodes <-  getNodes
                if n == length nodes
                 then do
                   r <- liftIO $ readMVar reduceResults    !!> "end reduce"
                   liftIO $ print r
                   return r
                 else empty

   in reducer <|> shuffler
   where
     atype ::DDS(M.Map k (V.Vector a)) ->  a
     atype = undefined -- type level

     paramOf  :: DDS (M.Map k (V.Vector a)) -> ReduceChunk( k,  a)
     paramOf = undefined -- type level

--     groupByKey :: Ord k2 => [(k2, v2)] -> M.Map k2 [v2]
--     groupByKey = M.fromListWith (++) . map (second return)

parallelize :: Loggable b => (a -> Cloud b) -> [a] -> Cloud b
parallelize f xs=  foldl (<|>) empty $ map  f xs

getPartitionData :: Loggable a => PartRef a   -> Cloud  a
getPartitionData (Ref node path save)  = local $ do
    Just (Part _ _ _ xs) <- liftIO $ atomically
                                   $ readDBRef
                                   $ getDBRef
                                   $ keyp path save

    return xs  -- !> "getPartitionData"

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
   nodes <- local getNodes      !> "DISTRIBUTE"
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
generateRef node x= local . liftIO $ do
       temp <- getTempName
       let reg=  Part node temp True  x
       atomically $ newDBRef reg
       return $ getRef reg

getRef (Part n t s x)= Ref n t s

getTempName :: IO String
getTempName=  ("DDS" ++) <$> replicateM  5 (randomRIO ('a','z'))


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


