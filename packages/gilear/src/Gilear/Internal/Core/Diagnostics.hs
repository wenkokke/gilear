module Gilear.Internal.Core.Diagnostics where

import Data.Text (Text)
import Data.IntervalMap.Strict (IntervalMap)
import Data.IntervalMap.Strict qualified as IM
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
