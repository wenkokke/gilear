{-# LANGUAGE CPP #-}

module Main where

import Data.List (intersperse)
import Distribution.Simple (Args, UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.PreProcess (PPSuffixHandler, PreProcessor (..), knownSuffixHandlers, mkSimplePreProcessor, unsorted)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.PreProcess.Types (Suffix (..))
#endif
import Distribution.Simple.Program (Program, runDbProgram, simpleProgram)
import Distribution.Simple.Setup (BuildFlags (buildVerbosity), ConfigFlags (configVerbosity), fromFlagOrDefault)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, intercalate, moreRecentFile, notice)
import Distribution.Types.BuildInfo (BuildInfo, hsSourceDirs)
import Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (LocalBuildInfo, withPrograms))
import Distribution.Types.PackageDescription (PackageDescription (PackageDescription, extraSrcFiles, extraTmpFiles))
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity (Verbosity, normal)

srcDir :: FilePath
srcDir = "src"

autogenDir :: FilePath
autogenDir = "autogen"

#if MIN_VERSION_Cabal(3,14,0)
agdaSuffixHandler :: PPSuffixHandler
agdaSuffixHandler = (Suffix "agda", preProcessAgda)
#else
agdaSuffixHandler :: PPSuffixHandler
agdaSuffixHandler = ("agda", preProcessAgda)
#endif

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = \args configFlags -> do
          makeAutogenDir args configFlags
          preConf simpleUserHooks args configFlags
      , hookedPreProcessors =
          knownSuffixHandlers <> [agdaSuffixHandler]
      }

makeAutogenDir :: Args -> ConfigFlags -> IO ()
makeAutogenDir _args configFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  notice verbosity $ "Create directory for generated modules: " ++ autogenDir
  createDirectoryIfMissingVerbose verbosity True autogenDir

preProcessAgda :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
preProcessAgda buildInfo localBuildInfo componentLocalBuildInfo =
  PreProcessor
    { platformIndependent = True
    , runPreProcessor = mkSimplePreProcessor agdaPreprocessor
    , ppOrdering = unsorted
    }
 where
  agdaPreprocessor :: FilePath -> FilePath -> Verbosity -> IO ()
  agdaPreprocessor src dst verbosity = do
    let LocalBuildInfo{withPrograms} = localBuildInfo
    notice verbosity $ "agda2hs: " <> src <> " -> " <> dst
    let args = ["--config=rewrite-rules.yaml", src]
    runDbProgram verbosity agda2hsProgram withPrograms args

agda2hsProgram :: Program
agda2hsProgram = simpleProgram "agda2hs"
