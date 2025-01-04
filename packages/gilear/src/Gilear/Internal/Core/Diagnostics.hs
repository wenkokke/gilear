module Gilear.Internal.Core.Diagnostics where

import Data.IntervalMap.Strict (IntervalMap)
import Data.IntervalMap.Strict qualified as IM
import Data.Text (Text)
import Gilear.Internal.Core.Location (ByteIndex, ByteRange, Range, byteRangeToInterval, rangeToByteRange)

data Severity
  = Error
  | Warning
  | Information
  | Hint

data Diagnostic = Diagnostic
  { diagnosticRange :: Range
  , diagnosticSeverity :: Severity
  , diagnosticMessage :: Text
  }

newtype Diagnostics = Diagnostics {unDiagnostics :: IntervalMap ByteIndex Diagnostic}

empty :: Diagnostics
empty = Diagnostics IM.empty

insert :: Diagnostic -> Diagnostics -> Diagnostics
insert diagnostic diagnostics =
  Diagnostics (IM.insert interval diagnostic (unDiagnostics diagnostics))
 where
  interval = byteRangeToInterval (rangeToByteRange (diagnosticRange diagnostic))

deleteByRange :: ByteRange -> Diagnostics -> Diagnostics
deleteByRange byteRange diagnostics = Diagnostics (before <> after)
 where
  (before, _intersecting, after) =
    IM.splitIntersecting (unDiagnostics diagnostics) (byteRangeToInterval byteRange)
