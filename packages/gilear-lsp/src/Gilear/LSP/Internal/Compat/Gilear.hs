{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Gilear.LSP.Internal.Compat.Gilear where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Gilear.Internal.Core qualified as TC
import Gilear.Internal.Core.Diagnostics qualified as Gilear
import Gilear.Internal.Core.Location (Point (..))
import Gilear.Internal.Core.Location qualified as Gilear
import Gilear.LSP.Internal.Core (DiagnosticSource, LspTc)
import Gilear.LSP.Internal.Core qualified as Config
import Language.LSP.Diagnostics qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP

publishParserDiagnostics ::
  LogAction LspTc (WithSeverity Text) ->
  LSP.NormalizedUri ->
  Maybe Int32 ->
  LspTc ()
publishParserDiagnostics logger docUri docVersion = do
  -- Set the diagnostics source to "gilear"
  let source = Just . T.pack $ "gilear[parser]"
  -- Import the diagnostics from gilear
  gilearDiagnostics <- TC.getDiagnostics docUri
  -- Report the diagnostics for debugging purposes
  logger <& T.pack (show gilearDiagnostics) `WithSeverity` Debug
  -- Get the configuration parameters
  maxNumberOfProblems <- Config.maxNumberOfProblems <$> LSP.getConfig
  let lspDiagnostics = importDiagnostics source $ fromMaybe mempty gilearDiagnostics
  -- TODO(optimisation): directly notify client, instead of going via `LSP.publishDiagnostics`
  LSP.publishDiagnostics maxNumberOfProblems docUri docVersion lspDiagnostics

importDiagnostics :: DiagnosticSource -> Gilear.Diagnostics -> LSP.DiagnosticsBySource
importDiagnostics source diagnostics =
  LSP.partitionBySource (importDiagnostic source <$> Gilear.toList diagnostics)

importDiagnostic :: DiagnosticSource -> Gilear.Diagnostic -> LSP.Diagnostic
importDiagnostic source Gilear.Diagnostic{..} =
  LSP.Diagnostic
    { _range = importRange diagnosticRange
    , _severity = Just (importSeverity diagnosticSeverity)
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = source
    , _message = diagnosticMessage
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }

importSeverity :: Gilear.Severity -> LSP.DiagnosticSeverity
importSeverity = \case
  Gilear.Error -> LSP.DiagnosticSeverity_Error
  Gilear.Warning -> LSP.DiagnosticSeverity_Warning
  Gilear.Information -> LSP.DiagnosticSeverity_Information
  Gilear.Hint -> LSP.DiagnosticSeverity_Hint

{-| Convert an `LSP.Range` to a `Gilear.Range`.

    __Warning__: `Gilear.Point` stores the row/column data as `Word32`,
    whereas `LSP.Position` stores the row/column information as a
    **31-bit** words, so this conversion is lossy.
-}
importRange :: Gilear.Range -> LSP.Range
importRange Gilear.Range{..} =
  LSP.Range
    { _start = importPoint rangeStartPoint
    , _end = importPoint rangeEndPoint
    }

{-| Convert an `LSP.Position` to a `Gilear.Point`.

    __Warning__: `Gilear.Point` stores the row/column data as `Word32`,
    whereas `LSP.Position` stores the row/column information as a
    **31-bit** words, so this conversion is lossy.
-}
importPoint :: Gilear.Point -> LSP.Position
importPoint Gilear.Point{..} =
  LSP.Position (fromIntegral pointRow) (fromIntegral pointColumn)

-- | Convert an `LSP.Position` to a `Gilear.Point`.
exportPoint :: LSP.Position -> Point
exportPoint (LSP.Position row column) =
  Point (fromIntegral row) (fromIntegral column)
