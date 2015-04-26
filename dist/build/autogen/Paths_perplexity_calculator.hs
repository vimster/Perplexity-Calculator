module Paths_perplexity_calculator (
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

bindir     = "C:\\Users\\Marvin\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Marvin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\perplexity-calculator-0.1.0.0"
datadir    = "C:\\Users\\Marvin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\perplexity-calculator-0.1.0.0"
libexecdir = "C:\\Users\\Marvin\\AppData\\Roaming\\cabal\\perplexity-calculator-0.1.0.0"
sysconfdir = "C:\\Users\\Marvin\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "perplexity_calculator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "perplexity_calculator_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "perplexity_calculator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "perplexity_calculator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "perplexity_calculator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
