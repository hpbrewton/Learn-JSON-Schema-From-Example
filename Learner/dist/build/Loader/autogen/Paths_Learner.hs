{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Learner (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/hb/.cabal/bin"
libdir     = "/Users/hb/.cabal/lib/x86_64-osx-ghc-8.6.5/Learner-0.1.0.0-16fESEBtPDN2a15ta1OpgG-Loader"
dynlibdir  = "/Users/hb/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/hb/.cabal/share/x86_64-osx-ghc-8.6.5/Learner-0.1.0.0"
libexecdir = "/Users/hb/.cabal/libexec/x86_64-osx-ghc-8.6.5/Learner-0.1.0.0"
sysconfdir = "/Users/hb/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Learner_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Learner_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Learner_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Learner_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Learner_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Learner_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
