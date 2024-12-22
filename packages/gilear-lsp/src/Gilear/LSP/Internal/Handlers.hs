{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Gilear.LSP.Internal.Handlers where

import Colog.Core (LogAction, WithSeverity)
import Control.Lens ((^.))
import Data.Text (Text)
import Gilear.LSP.Internal.Core (Config, LSPTC)
import Language.LSP.Protocol.Lens (HasParams (..), HasTextDocument (..), HasUri (..))
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types (ClientCapabilities, Null (..), type (|?) (..))
import Language.LSP.Server (LspM)
import Language.LSP.Server qualified as LSP

handlers ::
  (m ~ LspM Config) =>
  LogAction m (WithSeverity Text) ->
  ClientCapabilities ->
  LSP.Handlers LSPTC
handlers _clientCapabilities _logger =
  mconcat
    [ initializedHandler
    , textDocumentDidOpenHandler
    , textDocumentDidSaveHandler
    , textDocumentDidChangeHandler
    , textDocumentDidCloseHandler
    , workspaceDidChangeConfiguration
    ]

initializedHandler :: LSP.Handlers LSPTC
initializedHandler =
  LSP.notificationHandler SMethod_Initialized $ \_notification -> do
    pure ()

textDocumentDidOpenHandler :: LSP.Handlers LSPTC
textDocumentDidOpenHandler =
  LSP.notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
    let _uri = notification ^. params . textDocument . uri
    pure ()

textDocumentDidSaveHandler :: LSP.Handlers LSPTC
textDocumentDidSaveHandler =
  LSP.notificationHandler SMethod_TextDocumentDidSave $ \notification -> do
    let _uri = notification ^. params . textDocument . uri
    pure ()

textDocumentSemanticTokensFullHandler :: LSP.Handlers LSPTC
textDocumentSemanticTokensFullHandler =
  LSP.requestHandler SMethod_TextDocumentSemanticTokensFull $ \request responder -> do
    let _uri = request ^. params . textDocument . uri
    responder . Right . InR $ Null

textDocumentDidChangeHandler :: LSP.Handlers LSPTC
textDocumentDidChangeHandler =
  LSP.notificationHandler SMethod_TextDocumentDidChange $ \notification -> do
    let _uri = notification ^. params . textDocument . uri
    pure ()

textDocumentDidCloseHandler :: LSP.Handlers LSPTC
textDocumentDidCloseHandler =
  LSP.notificationHandler SMethod_TextDocumentDidClose $ \notification -> do
    let _uri = notification ^. params . textDocument . uri
    pure ()

workspaceDidChangeConfiguration :: LSP.Handlers LSPTC
workspaceDidChangeConfiguration =
  LSP.notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_notification ->
    pure ()
