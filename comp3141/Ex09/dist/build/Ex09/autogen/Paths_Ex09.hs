{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Ex09 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/import/ravel/3/z5161462/.cabal/bin"
libdir     = "/import/ravel/3/z5161462/.cabal/lib/i386-linux-ghc-8.2.2/Ex09-1.0-Dp61wTd461rAmnn8kM8VJU-Ex09"
dynlibdir  = "/import/ravel/3/z5161462/.cabal/lib/i386-linux-ghc-8.2.2"
datadir    = "/import/ravel/3/z5161462/.cabal/share/i386-linux-ghc-8.2.2/Ex09-1.0"
libexecdir = "/import/ravel/3/z5161462/.cabal/libexec/i386-linux-ghc-8.2.2/Ex09-1.0"
sysconfdir = "/import/ravel/3/z5161462/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Ex09_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Ex09_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Ex09_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Ex09_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ex09_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ex09_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
