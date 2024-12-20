#!/usr/bin/env cabal
{- cabal:
build-depends:
    base  >=4
  , Cabal >=2.0.0.2
default-language:   Haskell2010
ghc-options:        -Wall
-}
{-# LANGUAGE NamedFieldPuns #-}

import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Program (Program, simpleProgram, runDbProgram)
import Distribution.Simple.Setup (BuildFlags (buildVerbosity), fromFlagOrDefault)
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
    let LocalBuildInfo {withPrograms} = localBuildInfo
    runDbProgram verbosity npmProgram withPrograms ["exec", "--yes", "--package=tree-sitter-cli", "tree-sitter", "generate"]

npmProgram :: Program
npmProgram = simpleProgram "npm"
