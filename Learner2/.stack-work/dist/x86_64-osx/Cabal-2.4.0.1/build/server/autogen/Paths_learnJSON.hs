{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_learnJSON (
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

bindir     = "/Users/hb/latlib/Learner2/.stack-work/install/x86_64-osx/9831083565ca0bd0efcda07202a74c5edbf8575c2c49e0f202dc62c829b8db1d/8.6.5/bin"
libdir     = "/Users/hb/latlib/Learner2/.stack-work/install/x86_64-osx/9831083565ca0bd0efcda07202a74c5edbf8575c2c49e0f202dc62c829b8db1d/8.6.5/lib/x86_64-osx-ghc-8.6.5/learnJSON-0.1.0.0-AfZr6s8TT2GDY4SVjneUQi-server"
dynlibdir  = "/Users/hb/latlib/Learner2/.stack-work/install/x86_64-osx/9831083565ca0bd0efcda07202a74c5edbf8575c2c49e0f202dc62c829b8db1d/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/hb/latlib/Learner2/.stack-work/install/x86_64-osx/9831083565ca0bd0efcda07202a74c5edbf8575c2c49e0f202dc62c829b8db1d/8.6.5/share/x86_64-osx-ghc-8.6.5/learnJSON-0.1.0.0"
libexecdir = "/Users/hb/latlib/Learner2/.stack-work/install/x86_64-osx/9831083565ca0bd0efcda07202a74c5edbf8575c2c49e0f202dc62c829b8db1d/8.6.5/libexec/x86_64-osx-ghc-8.6.5/learnJSON-0.1.0.0"
sysconfdir = "/Users/hb/latlib/Learner2/.stack-work/install/x86_64-osx/9831083565ca0bd0efcda07202a74c5edbf8575c2c49e0f202dc62c829b8db1d/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "learnJSON_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "learnJSON_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "learnJSON_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "learnJSON_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "learnJSON_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "learnJSON_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
