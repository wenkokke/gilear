#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base   >=4.13 && <5
  , Cabal  >=3.0  && <4
default-language:   GHC
ghc-options:        -Wall
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.PreProcess (PPSuffixHandler, PreProcessor (..), knownSuffixHandlers, mkSimplePreProcessor, unsorted)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.PreProcess.Types (Suffix (..))
#endif
import Distribution.Simple.Program (Program, runDbProgram, simpleProgram)
import Distribution.Simple.Utils (notice)
import Distribution.Types.BuildInfo (BuildInfo)
import Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (LocalBuildInfo, withPrograms))
import Distribution.Verbosity (Verbosity)

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
      { hookedPreProcessors =
          knownSuffixHandlers <> [agdaSuffixHandler]
      }

preProcessAgda :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
preProcessAgda _buildInfo localBuildInfo _componentLocalBuildInfo =
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
