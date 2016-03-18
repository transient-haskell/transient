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
import Data.TCache
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

          r <- reduce  (+) . cmap separate . distribute $ V.fromList [1 .. 100 :: Int]

          lliftIO $  putStr "====================>" >> print r


runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()


separate x =
  case rem x 2 of
      0 -> ("even",x)
      _ -> ("odd",x)





