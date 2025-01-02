{-# LANGUAGE RecordWildCards #-}
module Gilear.Internal.Core.TextEdit where

import Data.Text (Text)
import Gilear.Internal.Core.Location (Point, pointToPosition, positionToPoint)
import Data.Text.Mixed.Rope (Rope)
import TreeSitter (InputEncoding)
import TreeSitter qualified as TS
import Data.Text.Mixed.Rope qualified as Rope
import Gilear.Internal.Core.Cache (CacheItem (..))

data TextEdit = TextEdit
  { editStart :: Point
  , editOldEnd :: Point
  , editNewText :: Text
  }

applyTextEditToCacheItem :: InputEncoding -> [TextEdit] -> CacheItem -> IO CacheItem
applyTextEditToCacheItem encoding = go
 where
  go :: [TextEdit] -> CacheItem -> IO CacheItem
  go [] cacheItem = pure cacheItem
  go (edit : edits) (CacheItem {itemRope = oldRope, itemTree = mutTree, ..}) = do
    let (newRope, inputEdit) = applyTextEditToRope encoding edit oldRope
    TS.treeEdit mutTree inputEdit
    go edits (CacheItem {itemRope = newRope, itemTree = mutTree, ..})

applyTextEditToRope :: InputEncoding -> TextEdit -> Rope -> (Rope, TS.InputEdit)
applyTextEditToRope encoding edit oldRope = (newRope, inputEdit)
  where
  -- Compute 'Rope' length in bytes depending on encoding
  lengthInBytes :: Rope -> Word
  lengthInBytes = case encoding of
    TS.InputEncodingUTF8 -> Rope.utf8Length
    TS.InputEncodingUTF16 -> Rope.utf16Length
  -- Unpack the edit
  (TextEdit startPoint oldEndPoint newText) = edit
  -- Compute 'startPosition' from 'startPoint' and 'oldEndPosition' from 'oldEndPoint'
  startPosition = pointToPosition startPoint
  oldEndPosition = pointToPosition oldEndPoint
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
