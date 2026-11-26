{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_laboratorio3 (
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

bindir     = "C:\\Users\\rialj\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\rialj\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5\\laboratorio3-0.1.0.0-Kzcb21XoeVDFiMMicNQLsK"
dynlibdir  = "C:\\Users\\rialj\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\rialj\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5\\laboratorio3-0.1.0.0"
libexecdir = "C:\\Users\\rialj\\AppData\\Roaming\\cabal\\laboratorio3-0.1.0.0-Kzcb21XoeVDFiMMicNQLsK\\x86_64-windows-ghc-8.6.5\\laboratorio3-0.1.0.0"
sysconfdir = "C:\\Users\\rialj\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "laboratorio3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "laboratorio3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "laboratorio3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "laboratorio3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "laboratorio3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "laboratorio3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
