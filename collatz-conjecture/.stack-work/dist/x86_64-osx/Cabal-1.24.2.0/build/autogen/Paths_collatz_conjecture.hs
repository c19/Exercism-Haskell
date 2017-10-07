{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_collatz_conjecture (
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
version = Version [1,1,1,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/c19/Documents/projects/exercism/haskell/haskell/collatz-conjecture/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/bin"
libdir     = "/Users/c19/Documents/projects/exercism/haskell/haskell/collatz-conjecture/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/lib/x86_64-osx-ghc-8.0.2/collatz-conjecture-1.1.1.1-4Q7m0FsPMUFufUXTepMSJ"
dynlibdir  = "/Users/c19/Documents/projects/exercism/haskell/haskell/collatz-conjecture/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/c19/Documents/projects/exercism/haskell/haskell/collatz-conjecture/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/share/x86_64-osx-ghc-8.0.2/collatz-conjecture-1.1.1.1"
libexecdir = "/Users/c19/Documents/projects/exercism/haskell/haskell/collatz-conjecture/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/libexec"
sysconfdir = "/Users/c19/Documents/projects/exercism/haskell/haskell/collatz-conjecture/.stack-work/install/x86_64-osx/lts-8.21/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "collatz_conjecture_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "collatz_conjecture_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "collatz_conjecture_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "collatz_conjecture_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "collatz_conjecture_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "collatz_conjecture_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
