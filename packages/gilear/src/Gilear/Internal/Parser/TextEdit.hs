{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Gilear.Internal.Parser.TextEdit (
  TextEdit (..),
  lengthInBytes,
  applyTextEditToItem,
) where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope
import Data.Word (Word32)
import Gilear.Internal.Core (MonadTc)
import Gilear.Internal.Core.Diagnostics qualified as D
import Gilear.Internal.Core.Location (ByteRange (..), Point, pointToPosition, positionToPoint)
import Gilear.Internal.Parser.Cache (ParserCacheItem (..))
import Text.Printf (printf)
import TreeSitter (InputEncoding)
import TreeSitter qualified as TS

data TextEdit = TextEdit
  { editStart :: Point
  , editOldEnd :: Point
  , editNewText :: Text
  }

applyTextEditToItem ::
  (MonadTc uri m) =>
  LogAction m (WithSeverity Text) ->
  InputEncoding ->
  [TextEdit] ->
  ParserCacheItem ->
  m ParserCacheItem
applyTextEditToItem logger encoding = go
 where
  -- go :: [TextEdit] -> CacheItem -> m CacheItem
  go [] item = pure item
  go (edit : edits) (ParserCacheItem{itemRope = oldRope, itemTree = mutTree, itemDiag = oldDiag, ..}) = do
    let (newRope, inputEdit) = applyTextEditToRope encoding edit oldRope
    liftIO (TS.treeEdit mutTree inputEdit)
    -- Delete any diagnostics out-of-bounds diagnostics
    let oldToNewEnd = oldEndByteToNewEndByteRange encoding oldRope newRope
    let deleteOoBDiag = maybe id D.deleteByRange oldToNewEnd
    for_ oldToNewEnd $ \(ByteRange oldEnd newEnd) ->
      logger <& T.pack (printf "delete %d:%d (%s)" oldEnd newEnd $ unwords [printf "%d:%d" (TS.rangeStartByte range) (TS.rangeEndByte range) | range <- D.diagnosticRange <$> D.toList oldDiag]) `WithSeverity` Debug
    -- Delete any diagnostics intersecting with the old range
    let oldByteRange = ByteRange (TS.inputEditStartByte inputEdit) (TS.inputEditOldEndByte inputEdit)
    for_ [oldByteRange] $ \(ByteRange oldStart oldEnd) ->
      logger <& T.pack (printf "delete %d:%d (%s)" oldStart oldEnd $ unwords [printf "%d:%d" (TS.rangeStartByte range) (TS.rangeEndByte range) | range <- D.diagnosticRange <$> D.toList oldDiag]) `WithSeverity` Debug
    let deleteOldDiag = D.deleteByRange oldByteRange
    -- Apply the above delete functions
    let newDiag = deleteOoBDiag . deleteOldDiag $ oldDiag
    go edits (ParserCacheItem{itemRope = newRope, itemTree = mutTree, itemDiag = newDiag, ..})

oldEndByteToNewEndByteRange :: InputEncoding -> Rope -> Rope -> Maybe ByteRange
oldEndByteToNewEndByteRange encoding oldRope newRope
  | oldRopeEnd > 0 && oldRopeEnd < newRopeEnd = Just $ ByteRange (oldRopeEnd - 1) (newRopeEnd - 1)
  | otherwise = Nothing
 where
  oldRopeEnd = lengthInBytes encoding oldRope
  newRopeEnd = lengthInBytes encoding newRope

applyTextEditToRope :: InputEncoding -> TextEdit -> Rope -> (Rope, TS.InputEdit)
applyTextEditToRope encoding edit oldRope = (newRope, inputEdit)
 where
  -- Unpack the edit
  (TextEdit startPoint oldEndPoint newText) = edit
  -- Compute 'startPosition' from 'startPoint' and 'oldEndPosition' from 'oldEndPoint'
  startPosition = pointToPosition startPoint
  oldEndPosition = pointToPosition oldEndPoint
  -- Compute 'startByte' as the length before 'startPosition'
  (oldRopeBeforeStart, _oldRopeAfterStart) = Rope.charSplitAtPosition startPosition oldRope
  startByte = lengthInBytes encoding oldRopeBeforeStart
  -- Compute 'oldEndByte' as the length before 'oldEndPosition'
  (oldRopeBeforeOldEnd, oldRopeAfterOldEnd) = Rope.charSplitAtPosition oldEndPosition oldRope
  oldEndByte = lengthInBytes encoding oldRopeBeforeOldEnd
  -- Compute 'newEndPosition/Byte' as 'startPosition/Byte' and 'newText' length
  insertedRope = Rope.fromText newText
  newEndByte = startByte + lengthInBytes encoding insertedRope
  newEndPosition = startPosition <> Rope.charLengthAsPosition insertedRope
  -- Edit Rope
  newRope = oldRopeBeforeStart <> Rope.fromText newText <> oldRopeAfterOldEnd
  -- Make InputEdit
  inputEdit =
    TS.InputEdit
      { TS.inputEditStartByte = startByte
      , TS.inputEditOldEndByte = oldEndByte
      , TS.inputEditNewEndByte = newEndByte
      , TS.inputEditStartPoint = positionToPoint startPosition
      , TS.inputEditOldEndPoint = positionToPoint oldEndPosition
      , TS.inputEditNewEndPoint = positionToPoint newEndPosition
      }

-- Compute 'Rope' length in bytes depending on encoding
lengthInBytes :: InputEncoding -> Rope -> Word32
lengthInBytes = \case
  TS.InputEncodingUTF8 -> fromIntegral . Rope.utf8Length
  TS.InputEncodingUTF16 -> fromIntegral . Rope.utf16Length
