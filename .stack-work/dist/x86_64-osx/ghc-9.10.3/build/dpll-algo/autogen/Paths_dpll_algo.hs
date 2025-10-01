{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_dpll_algo (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/z.dmitriy/dpll-algo/.stack-work/install/x86_64-osx/4b3660af6041f2cf119989dbe0c1b71fc09fb4b058ea1ae4ee59d1964346d19a/9.10.3/bin"
libdir     = "/Users/z.dmitriy/dpll-algo/.stack-work/install/x86_64-osx/4b3660af6041f2cf119989dbe0c1b71fc09fb4b058ea1ae4ee59d1964346d19a/9.10.3/lib/x86_64-osx-ghc-9.10.3-5528/dpll-algo-0.1.0.0-6bslCrLgRugAWVyXwD3MNZ-dpll-algo"
dynlibdir  = "/Users/z.dmitriy/dpll-algo/.stack-work/install/x86_64-osx/4b3660af6041f2cf119989dbe0c1b71fc09fb4b058ea1ae4ee59d1964346d19a/9.10.3/lib/x86_64-osx-ghc-9.10.3-5528"
datadir    = "/Users/z.dmitriy/dpll-algo/.stack-work/install/x86_64-osx/4b3660af6041f2cf119989dbe0c1b71fc09fb4b058ea1ae4ee59d1964346d19a/9.10.3/share/x86_64-osx-ghc-9.10.3-5528/dpll-algo-0.1.0.0"
libexecdir = "/Users/z.dmitriy/dpll-algo/.stack-work/install/x86_64-osx/4b3660af6041f2cf119989dbe0c1b71fc09fb4b058ea1ae4ee59d1964346d19a/9.10.3/libexec/x86_64-osx-ghc-9.10.3-5528/dpll-algo-0.1.0.0"
sysconfdir = "/Users/z.dmitriy/dpll-algo/.stack-work/install/x86_64-osx/4b3660af6041f2cf119989dbe0c1b71fc09fb4b058ea1ae4ee59d1964346d19a/9.10.3/etc"

getBinDir     = catchIO (getEnv "dpll_algo_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "dpll_algo_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "dpll_algo_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "dpll_algo_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dpll_algo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dpll_algo_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
