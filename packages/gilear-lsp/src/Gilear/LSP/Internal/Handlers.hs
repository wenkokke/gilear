{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Gilear.LSP.Internal.Handlers (
  handlers,
) where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Writer (MonadWriter (..), Writer, runWriter)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor (..))
import Data.Monoid (Any (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Gilear.Internal.Core qualified as Tc
import Gilear.Internal.Parser (InputEncoding (..))
import Gilear.Internal.Parser qualified as Tc
import Gilear.Internal.Parser.TextEdit (TextEdit (..))
import Gilear.LSP.Internal.Compat.Gilear qualified as Gilear
import Gilear.LSP.Internal.Core (LspTc)
import Language.LSP.Protocol.Lens (HasContentChanges (..), HasEnd (..), HasParams (..), HasRange (..), HasStart (..), HasText (..), HasTextDocument (..), HasUri (..))
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types (ClientCapabilities, TextDocumentContentChangeEvent (..), TextDocumentContentChangePartial, toNormalizedUri, type (|?) (..))
import Language.LSP.Server qualified as Lsp
import Language.LSP.VFS (file_text, file_version)

handlers ::
  LogAction LspTc (WithSeverity Text) ->
  ClientCapabilities ->
  Lsp.Handlers LspTc
handlers logger clientCapabilities =
  mconcat
    [ initializedHandler
    , textDocumentDidOpenHandler
    , textDocumentDidSaveHandler
    , textDocumentDidChangeHandler
    , textDocumentDidCloseHandler
    , workspaceDidChangeConfiguration
    ]
 where
  initializedHandler :: Lsp.Handlers LspTc
  initializedHandler =
    Lsp.notificationHandler SMethod_Initialized $ \_notification -> do
      logger <& (T.pack "ClientCapabilities: " <> TL.toStrict (encodeToLazyText clientCapabilities)) `WithSeverity` Debug
      config <- Lsp.getConfig
      logger <& (T.pack "Config: " <> TL.toStrict (encodeToLazyText config)) `WithSeverity` Debug
      pure ()

  textDocumentDidOpenHandler :: Lsp.Handlers LspTc
  textDocumentDidOpenHandler =
    Lsp.notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
      let docUri = toNormalizedUri $ notification ^. params . textDocument . uri
      -- Try and get the virtual file for the uri:
      Lsp.getVirtualFile docUri >>= \case
        -- If the virtual file does not exist...
        Nothing ->
          -- TODO: Recover by reparsing file.
          logger <& T.pack ("No virtual file for " <> show docUri) `WithSeverity` Error
        Just docVirtualFile -> do
          let docRope = docVirtualFile ^. file_text
          _success <- Tc.documentOpen logger InputEncodingUTF8 docUri docRope
          pure ()

  textDocumentDidSaveHandler :: Lsp.Handlers LspTc
  textDocumentDidSaveHandler =
    Lsp.notificationHandler SMethod_TextDocumentDidSave $ \notification -> do
      let _uri = notification ^. params . textDocument . uri
      pure ()

  -- textDocumentSemanticTokensFullHandler :: Lsp.Handlers LspTc
  -- textDocumentSemanticTokensFullHandler =
  --   Lsp.requestHandler SMethod_TextDocumentSemanticTokensFull $ \request responder -> do
  --     let _uri = request ^. params . textDocument . uri
  --     responder . Right . InR $ Null

  textDocumentDidChangeHandler :: Lsp.Handlers LspTc
  textDocumentDidChangeHandler =
    Lsp.notificationHandler SMethod_TextDocumentDidChange $ \notification -> do
      let docUri = toNormalizedUri $ notification ^. params . textDocument . uri
      let docChanges = notification ^. params . contentChanges
      -- Try and get the virtual file for the uri:
      Lsp.getVirtualFile docUri >>= \case
        -- If the virtual file does not exist...
        Nothing ->
          -- TODO: Recover by reparsing file.
          logger <& T.pack ("No virtual file for " <> show docUri) `WithSeverity` Error
        Just docVirtualFile -> do
          -- If there were any whole-document changes...
          let (docChangesPartial, docChangesWholeDocument) = partialDocumentContentChanges docChanges
          let docTextEdits = changePartialToTextEdit <$> docChangesPartial
          let docRope = docVirtualFile ^. file_text
          success <-
            if docChangesWholeDocument
              -- ... try and parse the whole document from the VFS
              then Tc.documentChangeWholeDocument logger InputEncodingUTF8 docUri docRope
              -- Otherwise, parse the document incrementally...
              else Tc.documentChangePartial logger InputEncodingUTF8 docUri docRope docTextEdits
          when success $ do
            let docVersion = docVirtualFile ^. file_version
            Gilear.publishParserDiagnostics logger docUri (Just . fromIntegral $ docVersion)
            Tc.lookupCache docUri >>= \case
              Nothing -> pure ()
              Just _parserCacheItem ->
                -- logger <& T.pack (show parserCacheItem.itemAst) `WithSeverity` Debug
                pure ()

  textDocumentDidCloseHandler :: Lsp.Handlers LspTc
  textDocumentDidCloseHandler =
    Lsp.notificationHandler SMethod_TextDocumentDidClose $ \notification -> do
      let _uri = notification ^. params . textDocument . uri
      pure ()

  workspaceDidChangeConfiguration :: Lsp.Handlers LspTc
  workspaceDidChangeConfiguration =
    Lsp.notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_notification ->
      pure ()

changePartialToTextEdit :: TextDocumentContentChangePartial -> TextEdit
changePartialToTextEdit changePartial =
  TextEdit
    { editStart = Gilear.exportPoint $ changePartial ^. range . start
    , editOldEnd = Gilear.exportPoint $ changePartial ^. range . end
    , editNewText = changePartial ^. text
    }

{-| Get all `TextDocumentContentChangePartial` items.

    The `Bool` value is `True` if any of the `TextDocumentContentChangeEvent`
    items were `TextDocumentContentChangeWholeDocument` items.
-}
partialDocumentContentChanges ::
  [TextDocumentContentChangeEvent] ->
  ([TextDocumentContentChangePartial], Bool)
partialDocumentContentChanges = second getAny . runWriter . go
 where
  go :: [TextDocumentContentChangeEvent] -> Writer Any [TextDocumentContentChangePartial]
  go [] = pure []
  -- If the event is a partial change, add it to the list of partial changes:
  go ((TextDocumentContentChangeEvent (InL changePartial)) : changeEvents) =
    (changePartial :) <$> go changeEvents
  -- If the event is a whole-document change, fail:
  go (TextDocumentContentChangeEvent (InR _changeWholeDocument) : changeEvents) =
    tell (Any True) >> go changeEvents
