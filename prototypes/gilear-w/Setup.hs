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
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Distribution.Compat.Directory (doesPathExist, makeAbsolute)
import Distribution.Simple (Args, UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.PreProcess (PPSuffixHandler, PreProcessor (..), knownSuffixHandlers, mkSimplePreProcessor, unsorted)
import Distribution.Simple.Program (Program, runDbProgram, simpleProgram)
import Distribution.Simple.Setup (ConfigFlags (configVerbosity), fromFlagOrDefault)
import Distribution.Simple.Utils (die', notice, copyFileVerbose, createDirectoryIfMissingVerbose)
import Distribution.Types.BuildInfo (BuildInfo)
import Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (LocalBuildInfo, withPrograms))
import Distribution.Verbosity (Verbosity, normal)
import System.FilePath ((-<.>), (</>), splitDirectories, joinPath)

srcDir :: FilePath
srcDir = "src"

autogenDir :: FilePath
autogenDir = "autogen"

agdaLibrariesFile :: FilePath
agdaLibrariesFile = autogenDir </> "libraries"

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPreProcessors =
          knownSuffixHandlers <> [agdaSuffixHandler]
      }

agdaSuffixHandler :: PPSuffixHandler
agdaSuffixHandler = ("agda", preProcessAgda)

makeAutogenDir :: Verbosity -> IO ()
makeAutogenDir verbosity = do
  autogenDirExists <- doesPathExist autogenDir
  unless autogenDirExists $ do
    notice verbosity $ "Create directory for generated modules: " ++ autogenDir
    createDirectoryIfMissingVerbose verbosity True autogenDir

writeAgdaLibrariesFile :: Verbosity -> IO ()
writeAgdaLibrariesFile verbosity = do
  -- Ensure autogenDir exists
  makeAutogenDir verbosity
  -- Write Agda libraries file
  agdaLibrariesFileExists <- doesPathExist agdaLibrariesFile
  unless agdaLibrariesFileExists $ do
    agdaStdlibAgdaLibFile <- makeAbsolute "./vendor/agda-stdlib/standard-library.agda-lib"
    agda2hsAgdaLibFile <- makeAbsolute "./vendor/agda2hs/agda2hs.agda-lib"
    let agdaLibraries = unlines [agdaStdlibAgdaLibFile, agda2hsAgdaLibFile] <> "\n"
    notice verbosity $ "Write Agda libraries file: " <> agdaLibrariesFile <> "\n" <> agdaLibraries
    writeFile agdaLibrariesFile agdaLibraries

preProcessAgda :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
preProcessAgda _buildInfo localBuildInfo _componentLocalBuildInfo =
  PreProcessor
    { platformIndependent = True
    , runPreProcessor = mkSimplePreProcessor agdaPreprocessor
    , ppOrdering = unsorted
    }
 where
  agdaPreprocessor :: FilePath -> FilePath -> Verbosity -> IO ()
  agdaPreprocessor sourceFile targetFile verbosity = do
        -- Ensure autogenDir exists
        makeAutogenDir verbosity
        -- Ensure Agda libraries file exists
        writeAgdaLibrariesFile verbosity
        -- Compile Agda file
        let LocalBuildInfo{withPrograms} = localBuildInfo
        notice verbosity $ "agda2hs: Compile " <> sourceFile
        runDbProgram verbosity agda2hsProgram withPrograms $
          [ "--library-file=" <> agdaLibrariesFile
          , "--config=rewrite-rules.yaml"
          , "--out-dir=" <> autogenDir
          , sourceFile
          ]
        let sourceFileParts = splitDirectories sourceFile
        let sourceFileRel = joinPath (drop 1 (dropWhile (/= srcDir) sourceFileParts))
        let autogenFile = autogenDir </> sourceFileRel -<.> "hs"
        copyFileVerbose verbosity autogenFile targetFile

agda2hsProgram :: Program
agda2hsProgram = simpleProgram "agda2hs"
