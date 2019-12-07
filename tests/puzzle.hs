-- https://adventofcode.com/2019/day/4

module Main where

import Transient.Base
import Transient.Indeterminism
import Control.Monad.IO.Class
import Control.Monad (guard)



main= keep' $ freeThreads $ threads 4 $ do
    ns <- collect 0 $ puzzle 108457 562041
    liftIO $ print $ length ns

puzzle n1 n2= do

     a <- choose  [n1 `div` 100000..n2 `div` 100000]
     b <- choose [0..9]
     c <- choose [0..9]
     d <- choose [0..9]
     e <- choose [0..9]
     f <- choose [0..9]
     let sn= [a, b, c, d, e, f]
     let n= atoi sn 
     guard $ n < n2 && n > n1
     guard $ a==b || b == c || c ==d || d==e || e==f
     guard $ a <= b && b <= c && c <= d && d <=e && e <= f

     
   where
   atoi n= foldl (\x y-> x*10+y) 0 n
   
