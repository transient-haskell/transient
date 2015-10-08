{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable #-}

module DistrbDataSets where
import Transient.Base
import Transient.Move
import Transient.Indeterminism
import Transient.DDS
import Control.Applicative
import Control.Monad.IO.Class
import Data.Monoid

-- Calculates separately the number od odd and even numbers in a list using
-- Distributed Data Sets (DDS's)
main= do
     let numNodes = 5
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports
     addNodes nodes
     keep' $
       do runNodes nodes
          let cdata = distribute [1 .. 10000 :: Int]
          let cdata' = cmap (*3) cdata
          r <- reduce (sumOddEven 0 0) sumIt (0,0) cdata'
          liftIO $ print r
          exit
sumOddEven:: Int -> Int -> [Int] -> (Int,Int)
sumOddEven o e []= (o,e)
sumOddEven o e (x:xs)=
  if x `rem` 2== 0 then sumOddEven (o+1) e xs
    else sumOddEven o (e+1) xs

sumIt :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumIt (o,e) (o',e')= (o+o',e+e')

runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()




