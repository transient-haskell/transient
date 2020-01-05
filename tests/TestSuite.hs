#!/usr/bin/env ./execthirdlinedocker.sh
-- development
-- set -e  && docker run -it -v /c/Users/magocoal/OneDrive/Haskell/devel:/devel agocorona/transient:05-02-2017  bash -c "runghc  -j2 -isrc -i/devel/transient/src /devel/transient/tests/$1 $2 $3 $4"

-- compile and run within a docker image
-- set -e && executable=`basename -s .hs ${1}` &&  docker run -it -v $(pwd):/work agocorona/transient:05-02-2017  bash -c "ghc /work/${1} && /work/${executable} ${2} ${3}"


import qualified Prelude as Pr(return)
import Prelude hiding ((>>=),(>>),return)

import Transient.Base
import Transient.EVars
import Transient.Indeterminism

import System.Exit
import Data.Monoid
import Control.Applicative
import Control.Monad.State
import System.Random
import Control.Concurrent
import Control.Exception.Base
import Data.List


--instance Monoid Int where
--   mempty  = 0
--   mappend = (+)

main= do
   keep' $ do
       let -- genElem :: a -> TransIO a
           genElem x= do  -- generates synchronous and asynchronous results with various delays
                isasync <- liftIO randomIO
                delay   <- liftIO $ randomRIO (1, 1000)
                liftIO $ threadDelay delay
                if isasync then async $ return x else return x

       liftIO $ putStrLn "--Testing thread control + Monoid + Applicative + async + indetermism---"

       collect 100 $ do                                                    -- gather the result of 100 iterations
           i <-  threads 0 $ choose [1..100]                               -- test 100 times. 'loop' for 100 times
           nelems   <- liftIO $ randomRIO (1, 10)                          -- :: TransIO Int
           nthreads <- liftIO $ randomRIO (1,nelems)                       -- different numbers of threads
           r <- threads nthreads $ foldr (+) 0  $ map genElem  [1..nelems] -- sum sync and async results using applicative
           assert (r == sum[1..nelems]) $ return ()

       liftIO $ putStrLn "--------------checking  parallel execution, Alternative, events --------"
       ev <- newEVar
       r <-  collect 3 $ readEVar ev <|> ((choose [1..3] >>= writeEVar ev) >> stop)
       assert (sort r== [1,2,3]) $ return ()
      
       liftIO $ print "SUCCESS"
       exit ()

   exitSuccess
