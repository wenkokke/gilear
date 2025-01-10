{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Gilear.Internal.Parser (
  InputEncoding (..),
  documentOpen,
  documentChangeWholeDocument,
  documentChangePartial,
) where

import Colog.Core (LogAction, Severity (..), WithSeverity (WithSeverity), (<&))
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Writer (MonadWriter (..), WriterT, execWriterT)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lines (Position)
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope
import Gilear.Internal.Core (MonadTC, assertNoCacheItem, lookupCache, modifyCache_, withLanguage, withParser)
import Gilear.Internal.Core.Diagnostics (Diagnostic (..), Diagnostics)
import Gilear.Internal.Core.Diagnostics qualified as D
import Gilear.Internal.Core.Location (pointToPosition)
import Gilear.Internal.Parser.Cache (ParserCacheItem (..))
import Gilear.Internal.Parser.Cache qualified as Cache
import Gilear.Internal.Parser.TextEdit (TextEdit (..), applyTextEditToItem)
import Text.Printf (printf)
import TreeSitter (InputEncoding (..))
import TreeSitter qualified as TS

-- | Open a text document.
documentOpen ::
  (Show uri, Hashable uri, MonadTC uri m) =>
  LogAction m (WithSeverity Text) ->
  InputEncoding ->
  uri ->
  Rope ->
  m Bool
documentOpen logger encoding uri rope = do
  -- TODO(NO_IGNORE_ASSERTS):
  -- This handler assumes that the document is opened for the first time.
  -- Therefore, check that there are no known ropes or trees for this uri.
  assertNoCacheItem logger uri
  -- When receiving a whole-document text, there is no way to safely reuse
  -- any old tree, so we parse the file from scratch.
  documentChangeWholeDocument logger encoding uri rope

-- | Change the whole content of the text document.
documentChangeWholeDocument ::
  (Show uri, Hashable uri, MonadTC uri m) =>
  LogAction m (WithSeverity Text) ->
  InputEncoding ->
  uri ->
  Rope ->
  m Bool
documentChangeWholeDocument logger encoding uri rope = do
  -- Parse the whole document:
  maybeTree <- parseDocumentFromRope encoding Nothing rope
  case maybeTree of
    -- If parsing failed...
    Nothing -> do
      -- ... this must be due to a timeout, cancellation, or invalid language:
      let message = T.pack $ printf "failed to parse %s due to timeout, cancellation, or an invalid tree-sitter language" (show uri)
      logger <& WithSeverity message Warning
      modifyCache_ (Cache.delete uri)
      pure False
    -- If parsing succeeded...
    Just tree -> do
      -- ... update the tree
      -- TODO: do not update tree if it contains errors?
      modifyCache_ $ Cache.insert uri (ParserCacheItem rope tree D.empty)
      pure True

-- | Change part of the content of the text document.
documentChangePartial ::
  (Show uri, Hashable uri, MonadTC uri m) =>
  LogAction m (WithSeverity Text) ->
  InputEncoding ->
  uri ->
  Rope ->
  [TextEdit] ->
  m Bool
documentChangePartial logger encoding uri newRope edits = do
  lookupCache uri >>= \case
    -- If the old tree CANNOT be found...
    Nothing -> do
      -- ... report a warning
      let message = T.pack $ printf "did not find old tree for %s; parsing whole document" (show uri)
      logger <& WithSeverity message Warning
      -- ... and parse the entire document from scratch
      documentChangeWholeDocument logger encoding uri newRope

    -- If an old tree is found...
    Just cacheItem -> do
      -- ... edit the old tree with the input edits
      ParserCacheItem{itemRope = editedOldRope, itemTree = editedOldTree, itemDiag = oldDiag} <-
        applyTextEditToItem encoding edits cacheItem
      -- ... report a warning when the editedOldRope is distinct from the newRope
      when (newRope /= editedOldRope) $ do
        let message = T.pack $ printf "edited rope out of sync with new rope for %s" (show uri)
        logger <& WithSeverity message Warning
      -- ... parse the document directly from the rope
      maybeNewTree <- parseDocumentFromRope encoding (Just editedOldTree) newRope
      case maybeNewTree of
        -- If parsing failed due to timeout/cancellation/language...
        Nothing -> do
          -- ... report an error and delete the cache entry
          let message = T.pack $ printf "failed to parse %s due to timeout, cancellation, or an invalid tree-sitter language" (show uri)
          logger <& WithSeverity message Warning
          modifyCache_ (Cache.delete uri)
          pure False
        -- If parsing succeeded...
        Just newTree -> do
          -- Traverse the syntax tree searching for parse errors...
          rootNode <- liftIO (TS.treeRootNode newTree)
          newDiag <-
            execWriterT . depthFirst rootNode nodeHasChangeAndError $ \node -> do
              -- check if the current node is a "missing" node
              nodeIsMissing <- tellIfMissingSymbol node
              unless nodeIsMissing $ do
                -- check if the current node is a non-empty error leaf
                nodeIsUnexpectedChar <- tellIfUnexpectedChar newRope node
                unless nodeIsUnexpectedChar $ do
                  void $ tellIfError node
          -- ... update the cache item
          modifyCache_ . Cache.insert uri $
            ParserCacheItem
              { itemRope = newRope
              , itemTree = newTree
              , itemDiag = oldDiag <> newDiag
              }
          pure True

tellIfMissingSymbol ::
  (MonadTC uri m) =>
  TS.Node ->
  WriterT Diagnostics m Bool
tellIfMissingSymbol node = do
  nodeIsMissing <- liftIO (TS.nodeIsMissing node)
  if nodeIsMissing
    then do
      nodeSymbol <- liftIO (TS.nodeSymbol node)
      nodeString <- showSymbol nodeSymbol
      diagnosticRange <- liftIO (TS.nodeRange node)
      let diagnosticMessage = "Syntax Error: Missing '" <> nodeString <> "'"
      tell $ D.singleton Diagnostic{diagnosticSeverity = D.Error, ..}
      pure True
    else pure False

tellIfUnexpectedChar ::
  (MonadTC uri m) =>
  Rope ->
  TS.Node ->
  WriterT Diagnostics m Bool
tellIfUnexpectedChar rope node = do
  nodeIsError <- liftIO (TS.nodeIsError node)
  nodeChildCount <- liftIO (TS.nodeChildCount node)
  -- ... find the node range
  diagnosticRange <- liftIO (TS.nodeRange node)
  let TS.Range{..} = diagnosticRange
  -- If node is a non-empty error leaf...
  if nodeIsError && nodeChildCount == 0 && rangeStartPoint < rangeEndPoint
    then do
      -- ... find the unexpected character
      let unexpectedChar = charAt (pointToPosition rangeStartPoint) rope
      -- ... find the expected symbols
      expectedSymbols <- T.intercalate ", " <$> (traverse showSymbol =<< nodeExpectedSymbols node)
      -- ... format the diagnostic message
      let diagnosticMessage = "Syntax Error: Expected " <> expectedSymbols <> ", found '" <> unexpectedChar <> "'"
      tell $ D.singleton Diagnostic{diagnosticSeverity = D.Error, ..}
      pure True
    else pure False

tellIfError ::
  (MonadTC uri m) =>
  TS.Node ->
  WriterT Diagnostics m Bool
tellIfError node = do
  -- TODO: the errors reported by this function are wonky,
  --       but this is likely due to the grammar
  nodeIsError <- liftIO (TS.nodeIsError node)
  -- If node is an error node...
  if nodeIsError
    then do
      -- ... find the error range
      diagnosticRange <- liftIO (TS.nodeRange node)
      -- ... find the expected symbols
      expectedSymbols <- T.intercalate ", " <$> (traverse showSymbol =<< nodeExpectedSymbols node)
      -- ... format the diagnostic message
      let diagnosticMessage = T.pack "Syntax Error: Expected " <> expectedSymbols
      tell $ D.singleton Diagnostic{diagnosticSeverity = D.Error, ..}
      pure True
    else pure False

nodeHasChangeAndError :: (MonadTC uri m) => TS.Node -> m Bool
nodeHasChangeAndError node =
  liftIO $ not <$> ((&&) <$> TS.nodeHasChanges node <*> TS.nodeHasError node)

showSymbol :: (MonadTC uri m) => TS.Symbol -> m Text
showSymbol symbol =
  withLanguage $ \language -> liftIO (languageShowSymbol language symbol)

languageShowSymbol :: TS.Language -> TS.Symbol -> IO Text
languageShowSymbol language symbol = do
  symbolName <- T.decodeUtf8 <$> TS.languageSymbolName language symbol
  symbolType <- TS.languageSymbolType language symbol
  pure $ if symbolType == TS.SymbolTypeAnonymous then "'" <> symbolName <> "'" else symbolName

nodeExpectedSymbols :: (MonadTC uri m) => TS.Node -> m [TS.Symbol]
nodeExpectedSymbols node = do
  stateId <- liftIO (TS.nodeParseState node)
  withLanguage $ \language ->
    liftIO (languageExpectedSymbols language stateId)

{-| @`languageExpectedSymbols` stateId@ returns the list of expected symbol names
  and types from a certain parse state.
-}
languageExpectedSymbols :: TS.Language -> TS.StateId -> IO [TS.Symbol]
languageExpectedSymbols language stateId = do
  stateCount <- TS.languageStateCount language
  if fromIntegral stateId >= stateCount
    then pure []
    else do
      lookaheadIterator <- TS.lookaheadIteratorNew language stateId
      let iter continue =
            if continue
              then do
                symbol <- TS.lookaheadIteratorCurrentSymbol lookaheadIterator
                success <- TS.lookaheadIteratorNext lookaheadIterator
                fmap (symbol :) (iter success)
              else pure []
      iter =<< TS.lookaheadIteratorNext lookaheadIterator

{-| @`depthFirst` node predGotoChild action@ performs a depth-first traversal
  of the tree, starting from @node@, skipping any subtree for which the
  function @predGotoChild@ returns @False@, and performing @action@ for each
  node that it visits.
-}
depthFirst :: (MonadIO m) => TS.Node -> (TS.Node -> m Bool) -> (TS.Node -> m ()) -> m ()
depthFirst node predGotoChild action = do
  treeCursor <- liftIO (TS.treeCursorNew node)
  let gotoFirstChild = do
        currentNode <- liftIO (TS.treeCursorCurrentNode treeCursor)
        shouldGotoChild <- predGotoChild currentNode
        if shouldGotoChild
          then do
            success <- liftIO (TS.treeCursorGotoFirstChild treeCursor)
            if success then gotoFirstChild else gotoNextSibling
          else gotoNextSibling
      gotoNextSibling = do
        currentNode <- liftIO (TS.treeCursorCurrentNode treeCursor)
        action currentNode
        success <- liftIO (TS.treeCursorGotoNextSibling treeCursor)
        if success
          then gotoFirstChild
          else gotoParent
      gotoParent = do
        success <- liftIO (TS.treeCursorGotoParent treeCursor)
        when success gotoNextSibling
  gotoFirstChild

{-| Parse a document from 'Rope'. This function uses the UTF8 encoding,
  since that is the encoding used internally by both 'Text' and 'Rope'.
-}
parseDocumentFromRope ::
  (MonadTC uri m) =>
  InputEncoding ->
  Maybe TS.Tree ->
  Rope ->
  m (Maybe TS.Tree)
parseDocumentFromRope encoding oldTree rope = do
  let input = inputFromRope encoding 4096 rope
  withParser $ \parser ->
    liftIO $ TS.parserParse parser oldTree input encoding

-- | Create a `TS.Input` function from a `Rope` with a desired encoding.
inputFromRope :: TS.InputEncoding -> Int -> Rope -> TS.Input
inputFromRope = \case
  InputEncodingUTF8 -> inputFromRopeUTF8
  InputEncodingUTF16 -> inputFromRopeUTF16
 where
  -- Create a `TS.Input` function from a `Rope` with UTF8 encoding.
  inputFromRopeUTF8 :: Int -> Rope -> TS.Input
  inputFromRopeUTF8 bufferSize rope _byteIndex point = do
    let afterPosition = snd $ Rope.charSplitAtPosition (pointToPosition point) rope
    let buffer = fillBuffer (Rope.lines afterPosition) bufferSize mempty
    pure . BSL.toStrict . BSB.toLazyByteString $ buffer
   where
    fillBuffer :: [Text] -> Int -> BSB.Builder -> BSB.Builder
    fillBuffer [] _bytesLeft buffer = buffer
    fillBuffer (line : rest) bytesLeft buffer = do
      let lineWithLF = if null rest then line else line <> T.singleton '\n'
      let lineBytes = T.encodeUtf8 lineWithLF
      if bytesLeft >= BS.length lineBytes
        then fillBuffer rest (bytesLeft - BS.length lineBytes) (buffer <> BSB.byteString lineBytes)
        else do
          let maxPrefixLineBytes = BS.take bytesLeft lineBytes
          let (validPrefixLen, _maybeUTF8State) = T.validateUtf8Chunk maxPrefixLineBytes
          let validPrefixLineBytes = BS.take validPrefixLen maxPrefixLineBytes
          buffer <> BSB.byteString validPrefixLineBytes

  -- Create a `TS.Input` function from a `Rope` with UTF16 encoding.
  inputFromRopeUTF16 :: Int -> Rope -> TS.Input
  inputFromRopeUTF16 bufferSize rope _byteIndex point = do
    let afterPosition = snd $ Rope.charSplitAtPosition (pointToPosition point) rope
    let buffer = fillBufferFast (Rope.lines afterPosition) bufferSize mempty
    pure . BSL.toStrict . BSB.toLazyByteString $ buffer
   where
    fillBufferFast :: [Text] -> Int -> BSB.Builder -> BSB.Builder
    fillBufferFast [] _bytesLeft buffer = buffer
    fillBufferFast (line : rest) bytesLeft buffer = do
      let lineWithLF = if null rest then line else line <> T.singleton '\n'
      -- NOTE:
      -- UTF16 documents without a byte-order-mark (BOM) should be interpreted
      -- as UTF16BE... and since we don't have a BOM, let's encode as UTF16BE!
      let lineBytes = T.encodeUtf16BE lineWithLF
      if bytesLeft >= BS.length lineBytes
        then fillBufferFast rest (bytesLeft - BS.length lineBytes) (buffer <> BSB.byteString lineBytes)
        else fillBufferSlow (T.unpack lineWithLF) bytesLeft buffer

    fillBufferSlow :: String -> Int -> BSB.Builder -> BSB.Builder
    fillBufferSlow [] _bytesLeft buffer = buffer
    fillBufferSlow (char : line) bytesLeft buffer = do
      let charBytes = T.encodeUtf16BE (T.singleton char)
      if bytesLeft >= BS.length charBytes
        then fillBufferSlow line (bytesLeft - BS.length charBytes) (buffer <> BSB.byteString charBytes)
        else buffer

-- | Internal helper: Get the character at a given `Position` in a `Rope`.
charAt :: Position -> Rope -> Text
charAt = ((Rope.toText . fst . Rope.charSplitAt 1 . snd) .) . Rope.charSplitAtPosition
