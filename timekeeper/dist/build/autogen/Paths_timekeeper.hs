module Paths_timekeeper (
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

bindir     = "C:\\Users\\Robin\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Robin\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.10.2\\timekeeper-0.1.0.0-I0K4IfAdQ4z3aa9Dl6Kqnh"
datadir    = "C:\\Users\\Robin\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.10.2\\timekeeper-0.1.0.0"
libexecdir = "C:\\Users\\Robin\\AppData\\Roaming\\cabal\\timekeeper-0.1.0.0-I0K4IfAdQ4z3aa9Dl6Kqnh"
sysconfdir = "C:\\Users\\Robin\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "timekeeper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "timekeeper_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "timekeeper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "timekeeper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "timekeeper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
