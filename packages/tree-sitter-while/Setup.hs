#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base   >=4.13 && <5
  , Cabal  >=2.0  && <3.16
default-language:   Haskell2010
ghc-options:        -Wall
-}
{-# LANGUAGE NamedFieldPuns #-}

import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Program (Program, runDbProgram, simpleProgram)
import Distribution.Simple.Setup (BuildFlags (buildVerbosity), fromFlagOrDefault)
import Distribution.System (Platform (..), OS (Windows), buildPlatform)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (LocalBuildInfo, withPrograms))
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Verbosity (normal)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { hookedPrograms = [npmProgram]
      , buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do
          treeSitterGenerate packageDescription localBuildInfo userHooks buildFlags
          buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
      }

treeSitterGenerate :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
treeSitterGenerate _packageDescription localBuildInfo _userHooks buildFlags = do
  let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
  let LocalBuildInfo{withPrograms} = localBuildInfo
  runDbProgram verbosity npmProgram withPrograms ["exec", "--yes", "tree-sitter", "generate"]

npmProgram :: Program
npmProgram = case buildPlatform of
  Platform _arch Windows -> simpleProgram "npm.exe"
  _otherwise -> simpleProgram "npm"
