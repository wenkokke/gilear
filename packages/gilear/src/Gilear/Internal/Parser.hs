{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Gilear.Internal.Parser (
  TextEdit (..),
  Position (..),
  InputEncoding (..),
  documentOpen,
  documentChangeWholeDocument,
  documentChangePartial,
) where

import Colog.Core (LogAction, Severity (..), WithSeverity (WithSeverity), (<&))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Hashable (Hashable)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lines (Position (..))
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope
import Gilear.Internal.Core (MonadTC, lookupCache, modifyCache_, withParser)
import Gilear.Internal.Parser.Cache (CacheItem (CacheItem))
import Gilear.Internal.Parser.Cache qualified as Cache
import Text.Printf (printf)
import TreeSitter (InputEncoding (..))
import TreeSitter qualified as TS

data TextEdit = TextEdit
  { textEditStartPosition :: Position
  , textEditOldEndPosition :: Position
  , textEditNewText :: Text
  }

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
    Nothing -> do
      -- Parsing failed due to timeout/cancellation/invalid language:
      let message = T.pack $ printf "failed to parse %s due to timeout, cancellation, or an invalid tree-sitter language" (show uri)
      logger <& WithSeverity message Warning
      modifyCache_ (Cache.delete uri)
      pure False
    -- If parsing succeeded...
    Just tree -> do
      -- ... update the tree
      -- TODO: do not update tree if it contains errors?
      modifyCache_ $ Cache.insert uri (CacheItem rope tree)
      pure True

-- | Change the part of the content of the text document.
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
      CacheItem editedOldRope editedOldTree <-
        liftIO $ editCacheItem encoding edits cacheItem
      -- ... report a warning when the editedOldRope is distinct from the newRope
      when (newRope /= editedOldRope) $ do
        let message = T.pack $ printf "edited rope out of sync with new rope for %s" (show uri)
        logger <& WithSeverity message Warning
      -- ... parse the document directly from the rope
      maybeNewTree <- parseDocumentFromRope encoding (Just editedOldTree) newRope
      case maybeNewTree of
        -- If parsing failed due to timeout/cancellation/language...
        Nothing -> do
          -- ... report an error and delete the tree
          let message = T.pack $ printf "failed to parse %s due to timeout, cancellation, or an invalid tree-sitter language" (show uri)
          logger <& WithSeverity message Warning
          modifyCache_ (Cache.delete uri)
          pure False
        -- If parsing succeeded...
        Just newTree -> do
          -- ... update the tree
          -- TODO: do not update tree if it contains errors?
          modifyCache_ $ Cache.insert uri (CacheItem newRope newTree)
          pure True

editCacheItem :: TS.InputEncoding -> [TextEdit] -> CacheItem -> IO CacheItem
editCacheItem encoding = go
 where
  go :: [TextEdit] -> CacheItem -> IO CacheItem
  go [] cacheItem = pure cacheItem
  go (edit : edits) (CacheItem oldRope mutTree) = do
    let (newRope, inputEdit) = editRope oldRope edit
    TS.treeEdit mutTree inputEdit
    go edits (CacheItem newRope mutTree)

  lengthInBytes :: Rope -> Word
  lengthInBytes = case encoding of
    TS.InputEncodingUTF8 -> Rope.utf8Length
    TS.InputEncodingUTF16 -> Rope.utf16Length

  editRope :: Rope -> TextEdit -> (Rope, TS.InputEdit)
  editRope oldRope edit = (newRope, inputEdit)
   where
    (TextEdit startPosition oldEndPosition newText) = edit
    -- Compute 'startByte' as the length before 'startPosition'
    (oldRopeBeforeStart, _oldRopeAfterStart) = Rope.charSplitAtPosition startPosition oldRope
    startByte = lengthInBytes oldRopeBeforeStart
    -- Compute 'oldEndByte' as the length before 'oldEndPosition'
    (oldRopeBeforeOldEnd, oldRopeAfterOldEnd) = Rope.charSplitAtPosition oldEndPosition oldRope
    oldEndByte = lengthInBytes oldRopeBeforeOldEnd
    -- Compute 'newEndPosition/Byte' as 'startPosition/Byte' and 'newText' length
    insertedRope = Rope.fromText newText
    newEndByte = startByte + fromIntegral (lengthInBytes insertedRope)
    newEndPosition = startPosition <> Rope.charLengthAsPosition insertedRope
    -- Edit Rope
    newRope = oldRopeBeforeStart <> Rope.fromText newText <> oldRopeAfterOldEnd
    -- Make InputEdit
    inputEdit =
      TS.InputEdit
        { TS.inputEditStartByte = fromIntegral startByte
        , TS.inputEditOldEndByte = fromIntegral oldEndByte
        , TS.inputEditNewEndByte = fromIntegral newEndByte
        , TS.inputEditStartPoint = positionToPoint startPosition
        , TS.inputEditOldEndPoint = positionToPoint oldEndPosition
        , TS.inputEditNewEndPoint = positionToPoint newEndPosition
        }

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
inputFromRope InputEncodingUTF8 = inputFromRopeUTF8
inputFromRope InputEncodingUTF16 = inputFromRopeUTF16

-- | Create a `TS.Input` function from a `Rope` with UTF8 encoding.
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

-- | Create a `TS.Input` function from a `Rope` with UTF16 encoding.
inputFromRopeUTF16 :: Int -> Rope -> TS.Input
inputFromRopeUTF16 bufferSize rope _byteIndex point = do
  let afterPosition = snd $ Rope.charSplitAtPosition (pointToPosition point) rope
  let buffer = fillBuffer (Rope.lines afterPosition) bufferSize mempty
  pure . BSL.toStrict . BSB.toLazyByteString $ buffer
 where
  fillBuffer :: [Text] -> Int -> BSB.Builder -> BSB.Builder
  fillBuffer [] _bytesLeft buffer = buffer
  fillBuffer (line : rest) bytesLeft buffer = do
    let lineWithLF = if null rest then line else line <> T.singleton '\n'
    -- NOTE:
    -- UTF16 documents without a byte-order-mark (BOM) should be interpreted
    -- as UTF16BE... and since we don't have a BOM, let's encode as UTF16BE!
    let lineBytes = T.encodeUtf16BE lineWithLF
    if bytesLeft >= BS.length lineBytes
      then fillBuffer rest (bytesLeft - BS.length lineBytes) (buffer <> BSB.byteString lineBytes)
      else fillBufferSlow (T.unpack lineWithLF) bytesLeft buffer

  fillBufferSlow :: String -> Int -> BSB.Builder -> BSB.Builder
  fillBufferSlow [] _bytesLeft buffer = buffer
  fillBufferSlow (char : line) bytesLeft buffer = do
    let charBytes = T.encodeUtf16BE (T.singleton char)
    if bytesLeft >= BS.length charBytes
      then fillBufferSlow line (bytesLeft - BS.length charBytes) (buffer <> BSB.byteString charBytes)
      else buffer

-- | Assert that there is cache item associated with the given @uri@.
assertNoCacheItem ::
  (Show uri, Hashable uri, MonadTC uri m) =>
  LogAction m (WithSeverity Text) ->
  uri ->
  m ()
assertNoCacheItem logger uri = do
  maybeCacheItem <- lookupCache uri
  when (isJust maybeCacheItem) $ do
    let message = T.pack $ printf "found cache item for %s" (show uri)
    logger <& WithSeverity message Warning

-- | Convert a 'Position' to a 'TS.Point'.
positionToPoint :: Position -> TS.Point
positionToPoint (Position row column) =
  TS.Point (fromIntegral row) (fromIntegral column)

-- | Convert a 'TS.Point' to a 'Position'.
pointToPosition :: TS.Point -> Position
pointToPosition (TS.Point row column) =
  Position (fromIntegral row) (fromIntegral column)
