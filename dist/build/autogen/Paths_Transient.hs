module Paths_transient (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\transient-0.1.0.0"
datadir    = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\transient-0.1.0.0"
libexecdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\transient-0.1.0.0"
sysconfdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "transient_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "transient_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "transient_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "transient_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "transient_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
