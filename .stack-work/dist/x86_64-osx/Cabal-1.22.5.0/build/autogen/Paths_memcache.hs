module Paths_memcache (
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

bindir     = "/Users/adinapoli/work/memcache-hs/.stack-work/install/x86_64-osx/lts-5.2/7.10.3/bin"
libdir     = "/Users/adinapoli/work/memcache-hs/.stack-work/install/x86_64-osx/lts-5.2/7.10.3/lib/x86_64-osx-ghc-7.10.3/memcache-0.1.0.0-3Z8qUpsEDiC53dYw3oq6HZ"
datadir    = "/Users/adinapoli/work/memcache-hs/.stack-work/install/x86_64-osx/lts-5.2/7.10.3/share/x86_64-osx-ghc-7.10.3/memcache-0.1.0.0"
libexecdir = "/Users/adinapoli/work/memcache-hs/.stack-work/install/x86_64-osx/lts-5.2/7.10.3/libexec"
sysconfdir = "/Users/adinapoli/work/memcache-hs/.stack-work/install/x86_64-osx/lts-5.2/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "memcache_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "memcache_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "memcache_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "memcache_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "memcache_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
