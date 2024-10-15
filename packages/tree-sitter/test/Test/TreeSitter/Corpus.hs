{-# LANGUAGE OverloadedStrings #-}

module Test.TreeSitter.Corpus where

import Control.Exception (Exception (..))
import Control.Monad (forM, unless)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Foreign.C.ConstPtr.Compat (ConstPtr)
import System.FilePath (makeRelative)
import System.FilePath.Glob qualified as Glob (globDir1)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
  assertBool,
  assertFailure,
  testCase,
 )
import TreeSitter.Corpus qualified as TSC
import TreeSitter.Internal qualified as TS
import TreeSitter.SExp (parseSExp, prettySExpDiff)

makeCorpusTests :: IO (ConstPtr tsLanguage) -> FilePath -> IO [TestTree]
makeCorpusTests languageConstructor corpusDirectory = do
  corpusFiles <- Glob.globDir1 "**/*.txt" corpusDirectory
  forM corpusFiles $ \corpusFile -> do
    let testName = makeRelative corpusDirectory corpusFile
    mentries <- TSC.readCorpusFile corpusFile
    pure $
      case mentries of
        Left errorMessage ->
          testCase testName (assertFailure errorMessage)
        Right entries ->
          testGroup testName (makeCorpusTest languageConstructor corpusFile <$> entries)

makeCorpusTest :: IO (ConstPtr tsLanguage) -> FilePath -> TSC.TestCase -> TestTree
makeCorpusTest languageConstructor corpusFile entry =
  testCase (TSC.nameString entry) $ do
    parser <- TS.parserNew
    language <- TS.unsafeToLanguage =<< languageConstructor
    success <- TS.parserSetLanguage parser language
    assertBool "parserSetLanguage: failed" success
    mtree <- TS.parserParseString parser Nothing $ TSC.codeString entry
    case mtree of
      Nothing -> assertFailure "parserParseString: failed"
      Just tree -> do
        node <- TS.treeRootNode tree
        sexpByteString <- TS.showNode node
        expectSExp <-
          either (assertFailure . displayException) pure $
            parseSExp corpusFile (TL.toStrict (TSC.sexp entry))
        actualSExp <-
          either (assertFailure . displayException) pure $
            parseSExp "tree-sitter" (TE.decodeUtf8 sexpByteString)
        unless (expectSExp == actualSExp) . assertFailure . unlines $
          [ "Found S-expression that was different from the expected result:"
          , ""
          , prettySExpDiff expectSExp actualSExp
          ]
