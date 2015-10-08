{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, MonadComprehensions  #-}
module MainStreamFiles where
import Transient.Base
import Transient.Stream.Resource
import Data.Char
import Control.Monad.IO.Class




-- continuos streaming version
-- Perform the same calculation but it does not stop, and the results are accumulated in in a mutable reference within the calling node,
-- so the precision in the value of pi is printed with more and more precision. every 1000 calculations.

-- Here instead of `collect` that finish the calculation when the number of samples has been reached, i use `group` which simply
-- group the number of results in a list and then sum of the list is returned to the calling node.

-- Since `group` do not finish the calculation, new sums are streamed from the nodes again and again.




main= keep . threads 0  $ do
         chunk <- sourceFile "../src/MainStreamFiles.hs"
         liftIO $ print chunk
         return $ map toUpper chunk
       `sinkFile` "outfile"

