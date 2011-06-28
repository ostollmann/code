module Paths_XGen (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/ost/bin/cabal/bin"
libdir     = "/home/ost/bin/cabal/lib/XGen-0.0.1/ghc-6.12.3"
datadir    = "/home/ost/bin/cabal/share/XGen-0.0.1"
libexecdir = "/home/ost/bin/cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "XGen_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "XGen_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "XGen_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "XGen_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
