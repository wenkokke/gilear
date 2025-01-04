module Gilear.Internal.Core.Location (
  Position (Position),
  Point (Point),
  positionToPoint,
  pointToPosition,
  ByteIndex,
  ByteRange (ByteRange, byteRangeStart, byteRangeEnd),
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

data ByteRange = ByteRange
  { byteRangeStart :: !ByteIndex
  , byteRangeEnd :: !ByteIndex
  }

byteRangeToInterval :: ByteRange -> Interval ByteIndex
byteRangeToInterval (ByteRange start end) = OpenInterval start end

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
