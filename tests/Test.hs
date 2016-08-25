{-# LANGUAGE   CPP #-}



import Transient.Base
import Transient.EVars
import Transient.Backtrack
import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class



main= keep $ do
    ev <- newEVar

    readers ev 100 <|> writers ev 100



readers ev n= foldr (<>) mempty $ take n $ repeat $ readEVar ev

writers ev n= foldr (<|>) empty $ take n $ repeat $ do
     waitEvents $ threadDelay 1000000
     writeEVar ev (1::Int)
     empty

instance Monoid Int where
  mempty= 0
  mappend= (+)
