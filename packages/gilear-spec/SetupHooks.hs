{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks (setupHooks) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Traversable (for)
import Distribution.Simple.Glob (globMatches, runDirFileGlob)
import Distribution.Simple.Glob.Internal (Glob (..), GlobPiece (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..), componentBuildInfo, mbWorkDirLBI)
import Distribution.Simple.Program (runProgramCwd)
import Distribution.Simple.Program.Db (lookupProgramByName)
import Distribution.Simple.SetupHooks
import Distribution.Utils.Path
import Distribution.Utils.ShortText (toShortText)
import System.FilePath (takeBaseName, takeDirectory, joinPath, pathSeparator)

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks = myBuildHooks
    }

myBuildHooks :: BuildHooks
myBuildHooks =
  noBuildHooks
    { preBuildComponentRules =
        Just $ rules (static ()) myBnfcRules
    }

type BnfcArgs = (Verbosity, Maybe (SymbolicPath CWD ('Dir Pkg)), ConfiguredProgram, Location, Location, String)

myBnfcRules :: PreBuildComponentInputs -> RulesM ()
myBnfcRules pbci = do
  let PreBuildComponentInputs{..} = pbci
  let TargetInfo{..} = targetInfo
  let buildInfo = componentBuildInfo targetComponent
  let mbWorkDir = mbWorkDirLBI localBuildInfo
  let verbosity = buildingWhatVerbosity buildingWhat
  let programDb = withPrograms localBuildInfo

  -- 1. Find BNFC in the Cabal program database
  for_ (lookupProgramByName "bnfc" programDb) $ \bnfc -> do
    -- 2. Define how to invoke BNFC.
    let bnfcCmd :: Location -> Location -> String -> Command BnfcArgs (IO ())
        bnfcCmd inputLoc outputPath grammarNameSpace  =
          mkCommand
            (static Dict)
            (static runBnfc)
            (verbosity, mbWorkDir, bnfc, inputLoc, outputPath, grammarNameSpace)

    -- 3. Search for "*.cf" files to pre-process in the source directories of the package.
    let glob = GlobDirRecursive [WildCard, Literal "cf"]
    lbnfFiles <-
      liftIO $ for (hsSourceDirs buildInfo) $ \srcDir -> do
        let root = interpretSymbolicPath mbWorkDir srcDir
        matches <- runDirFileGlob verbosity Nothing root glob
        pure
          [ Location srcDir (makeRelativePathEx match)
          | match <- globMatches matches
          ]

    -- 4. Add monitor for new "*.cf" files.
    addRuleMonitors [monitorFileGlobExistence $ RootedGlob FilePathRelative glob]

    -- 5. Declare rules, one for each module to be pre-processed.
    for_ (concat lbnfFiles) $ \inputLoc@(Location _ inputRelPath) -> do
      let outputBasePath = autogenComponentModulesDir localBuildInfo targetCLBI
      let outputBaseLoc = Location outputBasePath (coerceSymbolicPath sameDirectory)
      let grammarName = takeBaseName (getSymbolicPath inputRelPath)
      let grammarDirRelPath = takeDirectory (getSymbolicPath inputRelPath)
      let grammarNameSpace = toNameSpace grammarDirRelPath
      let outputLocs =
            fromList
              [ Location outputBasePath (makeRelativePathEx $ joinPath [grammarDirRelPath, grammarName, outputFileName])
              | outputFileName <- ["Abs.hs", "Print.hs", "Lex.x", "Par.y", "ErrM.hs", "Skel.hs"]
              ]
      registerRule_ (toShortText $ getSymbolicPath inputRelPath) $
        staticRule (bnfcCmd inputLoc outputBaseLoc grammarNameSpace) [] outputLocs

runBnfc :: BnfcArgs -> IO ()
runBnfc ( verbosity, mbWorkDir, bnfc, inputLoc, outputLoc, grammarNameSpace ) = do
  let inputPath = getSymbolicPath (location inputLoc)
  let outputPath = getSymbolicPath (location outputLoc)
  runProgramCwd verbosity mbWorkDir bnfc $
    [ "--haskell", "-d", "-p", grammarNameSpace, "--text-token", "--functor", "--generic", inputPath, "-o",  outputPath  ]

toNameSpace :: FilePath -> String
toNameSpace = replace pathSeparator '.'
 where
  replace :: Char -> Char -> String -> String
  replace a b = map (\c -> if c == a then b else c)
