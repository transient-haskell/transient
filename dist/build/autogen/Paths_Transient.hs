module Paths_Transient (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\Transient-0.1.0.0"
datadir    = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\Transient-0.1.0.0"
libexecdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\Transient-0.1.0.0"
sysconfdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Transient_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Transient_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Transient_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Transient_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Transient_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
