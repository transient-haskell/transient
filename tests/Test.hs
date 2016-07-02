{-# LANGUAGE   CPP #-}



import Transient.Move.Services
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class



main= do

    runCloudIO $ do
      runNodes [2000,2001]
      local $ option "start" "start"


      node <- initService  "ident" ("http://github.com/agocorona/transient", "MainStreamFiles")
      onAll . liftIO $ putStrLn $ "installed at" ++ show node


