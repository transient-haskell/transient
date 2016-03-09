{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable #-}

-- https://www.fpcomplete.com/user/agocorona/streaming-transient-effects-vi

module DistrbDataSets where
import Transient.Base
import Transient.Move
import Transient.Indeterminism
import Transient.DDS
import Control.Applicative
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Vector as V

-- Calculates separately the number od odd and even numbers in a list using
-- Distributed Data Sets (DDS's)
main= do
     let numNodes = 1
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports

     runCloud' $ do
          local $ addNodes nodes
          runNodes nodes
          let cdata = distribute $ V.fromList [1 .. 10000 :: Int]
          let cdata' = cmap separate cdata
          r <- reduce (+)  cdata'
          lliftIO $ print r
          local exit


sumIt :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumIt (o,e) (o',e')= (o+o',e+e')

runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()


separate x =
  case rem x 2 of
      0 -> ("even",x)
      _ -> ("odd",x)





