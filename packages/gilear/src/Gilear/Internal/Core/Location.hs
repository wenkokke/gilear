{-# LANGUAGE PatternSynonyms #-}

module Gilear.Internal.Core.Location (
  Point (..),
  positionToPoint,
  pointToPosition,
  ByteIndex,
  ByteRange (ByteRange, byteRangeStart, byteRangeEnd),
  byteIndexToByteRange,
  byteRangeToInterval,
  intervalToByteRange,
  Range (..),
  rangeToByteRange,
) where

import Data.IntervalMap.Strict (Interval (..))
import Data.Text.Lines (Position (Position))
import Data.Word (Word32)
import TreeSitter (Point (..), Range (..))

type ByteIndex = Word32

data ByteRange = UnsafeByteRange !ByteIndex !ByteIndex

byteRange :: ByteIndex -> ByteIndex -> ByteRange
byteRange x y = if x < y then UnsafeByteRange x y else UnsafeByteRange y x

pattern ByteRange :: ByteIndex -> ByteIndex -> ByteRange
pattern ByteRange{byteRangeStart, byteRangeEnd} <- UnsafeByteRange byteRangeStart byteRangeEnd
  where
    ByteRange byteRangeStart byteRangeEnd = byteRange byteRangeStart byteRangeEnd

{-# COMPLETE ByteRange #-}

instance Show ByteRange where
  showsPrec :: Int -> ByteRange -> ShowS
  showsPrec d (ByteRange start end) =
    showParen (d > 10) $
      showString "ByteRange " . showsPrec d start . showString " " . showsPrec d end

byteIndexToByteRange :: ByteIndex -> ByteRange
byteIndexToByteRange byteIndex = ByteRange byteIndex byteIndex

byteRangeToInterval :: ByteRange -> Interval ByteIndex
byteRangeToInterval (ByteRange start end) = ClosedInterval start end

intervalToByteRange :: Interval ByteIndex -> ByteRange
intervalToByteRange (IntervalCO start end) = ByteRange start (end - 1)
intervalToByteRange (ClosedInterval start end) = ByteRange (start + 1) (end - 1)
intervalToByteRange (OpenInterval start end) = ByteRange start end
intervalToByteRange (IntervalOC start end) = ByteRange (start + 1) end

rangeToByteRange :: Range -> ByteRange
rangeToByteRange Range{rangeStartByte, rangeEndByte} = ByteRange rangeStartByte rangeEndByte

{-| Convert a `Position` to a `Point`.

  __Warning__:
  `Position` and `Point` stores the row and column information as `Word` and
  `Word32`, respectively, so this is a lossy operation on architectures where
  the default word size is `Word64`.
-}
positionToPoint :: Position -> Point
positionToPoint (Position row column) =
  Point (fromIntegral row) (fromIntegral column)

-- | Convert a `Point` to a `Position`.
pointToPosition :: Point -> Position
pointToPosition (Point row column) =
  Position (fromIntegral row) (fromIntegral column)
