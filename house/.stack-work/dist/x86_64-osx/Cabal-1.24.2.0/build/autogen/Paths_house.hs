{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_house (
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
version = Version [0,1,0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/c19/Documents/projects/exercism/haskell/haskell/house/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/bin"
libdir     = "/Users/c19/Documents/projects/exercism/haskell/haskell/house/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/lib/x86_64-osx-ghc-8.0.2/house-0.1.0.2-HQG49pL7fU9KuPvuj9qRzx"
dynlibdir  = "/Users/c19/Documents/projects/exercism/haskell/haskell/house/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/c19/Documents/projects/exercism/haskell/haskell/house/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/share/x86_64-osx-ghc-8.0.2/house-0.1.0.2"
libexecdir = "/Users/c19/Documents/projects/exercism/haskell/haskell/house/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/libexec"
sysconfdir = "/Users/c19/Documents/projects/exercism/haskell/haskell/house/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "house_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "house_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "house_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "house_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "house_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "house_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
