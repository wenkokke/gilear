{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Exception (assert)
import Data.Diff.Myers (diffTextsToChangeEventsConsolidate)
import Data.Diff.Types qualified as Diff
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as L1
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Rope qualified as Rope
import Options.Applicative (
  Alternative (many),
  Parser,
  ParserInfo,
  execParser,
  help,
  helper,
  info,
  metavar,
  progDesc,
  strArgument,
 )
import System.Exit (exitSuccess)
import System.FilePath ((-<.>))
import System.IO (IOMode (..), withFile)
import TreeSitter.Gilear qualified as TS (tree_sitter_gilear)
import TreeSitter.Internal qualified as TS

parserParseText :: TS.Parser -> Maybe TS.Tree -> Text -> IO (Maybe TS.Tree)
parserParseText parser oldTree text =
  TS.parserParseByteStringWithEncoding parser oldTree (T.encodeUtf8 text) TS.InputEncodingUTF8

main :: IO ()
main = do
  cliOptions <- execParser cliOptionInfo
  -- Read the input files:
  files@(initialFile :| editedFiles) <- maybe exitSuccess pure . L1.nonEmpty . inputFiles $ cliOptions
  texts@(initialText :| editedTexts) <- traverse T.readFile files
  TS.withParser $ \parser -> do
    language <- TS.unsafeToLanguage =<< TS.tree_sitter_gilear
    success <- TS.parserSetLanguage parser language
    assert success $ do
      Just initialTree <- parserParseText parser Nothing initialText
      withFile (initialFile -<.> "dot") WriteMode $ TS.treePrintDotGraph initialTree
      treeRef <- newIORef initialTree
      let inputEdits = uncurry diffTextsToInputEdits <$> adjacentPairs texts
      for_ (zip (zip editedFiles editedTexts) inputEdits) $ \((editedFile, editedText), inputEdit) -> do
        oldTree <- readIORef treeRef
        for_ inputEdit $ TS.treeEdit oldTree
        Just newTree <- parserParseText parser (Just oldTree) editedText
        TS.unsafeTreeDelete oldTree
        writeIORef treeRef newTree
        withFile (editedFile -<.> "dot") WriteMode $ TS.treePrintDotGraph newTree
      tree <- readIORef treeRef
      TS.unsafeTreeDelete tree

adjacentPairs :: NonEmpty a -> [(a, a)]
adjacentPairs (x :| xs) = zip (x : xs) xs

diffTextsToInputEdits :: Text -> Text -> [TS.InputEdit]
diffTextsToInputEdits oldText newText =
  changeEventToInputEdit oldText <$> diffTextsToChangeEventsConsolidate oldText newText

changeEventToInputEdit :: Text -> Diff.ChangeEvent -> TS.InputEdit
changeEventToInputEdit oldText changeEvent =
  TS.InputEdit
    { TS.inputEditStartByte = fromIntegral startByte
    , TS.inputEditOldEndByte = fromIntegral oldEndByte
    , TS.inputEditNewEndByte = fromIntegral newEndByte
    , TS.inputEditStartPoint = positionToPoint startPosition
    , TS.inputEditOldEndPoint = positionToPoint oldEndPosition
    , TS.inputEditNewEndPoint = positionToPoint newEndPosition
    }
 where
  oldRope = Rope.fromText oldText
  newRope = Rope.fromText changeEvent.text
  startPointRow = fromIntegral changeEvent.range.rangeStart.positionLine
  startPointColumn = fromIntegral changeEvent.range.rangeStart.positionCh
  startPosition = Rope.Position startPointRow startPointColumn
  (oldRopeBeforeStart, _oldRopeAfterStart) = Rope.splitAtPosition startPosition oldRope
  startByte = Rope.length oldRopeBeforeStart
  oldEndPointRow = fromIntegral changeEvent.range.rangeEnd.positionLine
  oldEndPointColumn = fromIntegral changeEvent.range.rangeEnd.positionCh
  oldEndPosition = Rope.Position oldEndPointRow oldEndPointColumn
  (oldRopeBeforeEnd, _oldRopeAfterEnd) = Rope.splitAtPosition oldEndPosition oldRope
  oldEndByte = Rope.length oldRopeBeforeEnd
  newEndPosition = startPosition <> Rope.lengthAsPosition newRope
  newEndByte = startByte + Rope.length (Rope.fromText changeEvent.text)

positionToPoint :: Rope.Position -> TS.Point
positionToPoint position =
  TS.Point (fromIntegral position.posLine) (fromIntegral position.posColumn)

--------------------------------------------------------------------------------
-- Command-Line Options
--------------------------------------------------------------------------------

newtype CliOptions = CliOptions
  { inputFiles :: [FilePath]
  }

cliOptionInfo :: ParserInfo CliOptions
cliOptionInfo =
  info
    (helper <*> cliOptionParser)
    (progDesc "Generate DOT files from a sequence of Gilear programs.")

cliOptionParser :: Parser CliOptions
cliOptionParser =
  CliOptions <$> many inputFileArgument
 where
  inputFileArgument =
    strArgument . mconcat $
      [ metavar "INPUT_FILES"
      , help "Gilear source files."
      ]
