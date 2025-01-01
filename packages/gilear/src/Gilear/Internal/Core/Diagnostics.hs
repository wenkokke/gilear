{-# LANGUAGE DerivingStrategies #-}

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

deriving stock instance Show Severity

data Diagnostic = Diagnostic
  { diagnosticRange :: Range
  , diagnosticSeverity :: Severity
  , diagnosticMessage :: Text
  }

deriving stock instance Show Diagnostic

newtype Diagnostics = Diagnostics {unDiagnostics :: IntervalMap ByteIndex Diagnostic}

deriving newtype instance Show Diagnostics

deriving newtype instance Semigroup Diagnostics

deriving newtype instance Monoid Diagnostics

empty :: Diagnostics
empty = Diagnostics IM.empty

singleton :: Diagnostic -> Diagnostics
singleton diagnostic = Diagnostics $ IM.singleton interval diagnostic
 where
  interval = byteRangeToInterval (rangeToByteRange (diagnosticRange diagnostic))

null :: Diagnostics -> Bool
null = IM.null . unDiagnostics

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

toList :: Diagnostics -> [Diagnostic]
toList = IM.elems . unDiagnostics
