{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Gilear.LSP.Internal.Handlers where

import Colog.Core (LogAction, WithSeverity)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Gilear.Internal.Parser qualified as TC (parse)
import Gilear.LSP.Internal.Core (LSPTC)
import Language.LSP.Protocol.Lens (HasParams (..), HasTextDocument (..), HasUri (..))
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types (ClientCapabilities, TextDocumentItem (..), toNormalizedUri)
import Language.LSP.Server qualified as LSP
import TreeSitter qualified as TS
import Colog.Core.Action ((<&))
import Colog.Core.Severity (Severity (..), WithSeverity (..))
import Data.Text.Encoding qualified as T

handlers ::
  LogAction LSPTC (WithSeverity Text) ->
  ClientCapabilities ->
  LSP.Handlers LSPTC
handlers logger _clientCapabilities =
  mconcat
    [ initializedHandler
    , textDocumentDidOpenHandler
    , textDocumentDidSaveHandler
    , textDocumentDidChangeHandler
    , textDocumentDidCloseHandler
    , workspaceDidChangeConfiguration
    ]
 where
  initializedHandler :: LSP.Handlers LSPTC
  initializedHandler =
    LSP.notificationHandler SMethod_Initialized $ \_notification -> do
      pure ()

  textDocumentDidOpenHandler :: LSP.Handlers LSPTC
  textDocumentDidOpenHandler =
    LSP.notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
      let TextDocumentItem{_uri, _text} = notification ^. params . textDocument
      let normalizedUrl = toNormalizedUri _uri
      maybeTree <- TC.parse normalizedUrl _text
      case maybeTree of
        Nothing -> pure ()
        Just tree -> do
          rootNode <- liftIO $ TS.treeRootNode tree
          rootNodeByteString <-liftIO $ TS.showNode rootNode 
          let msg = T.decodeUtf8 rootNodeByteString
          logger <& msg `WithSeverity` Debug
          pure ()

  textDocumentDidSaveHandler :: LSP.Handlers LSPTC
  textDocumentDidSaveHandler =
    LSP.notificationHandler SMethod_TextDocumentDidSave $ \notification -> do
      let _uri = notification ^. params . textDocument . uri
      pure ()

  -- textDocumentSemanticTokensFullHandler :: LSP.Handlers LSPTC
  -- textDocumentSemanticTokensFullHandler =
  --   LSP.requestHandler SMethod_TextDocumentSemanticTokensFull $ \request responder -> do
  --     let _uri = request ^. params . textDocument . uri
  --     responder . Right . InR $ Null

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
