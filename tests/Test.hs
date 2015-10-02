{-# LANGUAGE MonadComprehensions, TypeSynonymInstances, ExistentialQuantification #-}
import Transient.Base
import Transient.Move
import Transient.Indeterminism
import Transient.Logged
import Network
import Data.IORef
import Control.Applicative
import System.Random
import Control.Monad.IO.Class
import GHC.Conc
import System.IO
import Control.Monad
import Data.Monoid
import System.Directory

data CloudArray a= Loggable a => CloudArray (TransIO [Elem a])
data Elem a= Ref Node Path | Local [a]
en caso de fallo de Node, se lanza un clustered en busca del path
   si solo uno lo tiene, se copia a otro
   se pone ese nodo de referencia en Ref

logging en fichero para recovery
como a¤adir al procesamiento remoto una parte del paso de reduce?

runAt node x >>= runAt node y ->  runAt node x>>=y


type Path=String

instance Functor  CloudArray where
   fmap f (CloudArray mx)= CloudArray $ do
        xs <- mx
        let rss = map (process f) xs
        return rss

process f (Local xs)= Local $ return $ map f xs
process f (Ref node path)= Ref node $ runAt node $ f1 path
    where
    f1 path=liftIO $ do
        str <- readFile path
        let cont= read str
        temp <- getTemporaryDirectory
        writeFile temp $ show $ fmap f cont

clusterPartition :: Loggable b => (a -> b) ->  [a] -> TransIO [b]
clusterPartition  f xs=do
   nodes <- getNodes
   let size= length xs `div` length nodes
   let chunks = partition size xs
   foldl (<>) mempty $ [ runAt node $ return $ map f chunk| (node,chunk) <- zip nodes chunks]


partition :: Int -> [e] -> [[e]]
partition i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []
