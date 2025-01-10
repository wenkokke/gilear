{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Gilear.LSP.Internal.Handlers where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Writer (MonadWriter (..), Writer, runWriter)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor (..))
import Data.Monoid (Any (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Gilear.Internal.Core qualified as TC
import Gilear.Internal.Parser (InputEncoding (..))
import Gilear.Internal.Parser qualified as TC
import Gilear.Internal.Parser.Cache qualified as TC
import Gilear.Internal.Parser.TextEdit (TextEdit (..))
import Gilear.LSP.Internal.Compat.Gilear qualified as Gilear
import Gilear.LSP.Internal.Core (LSPTC)
import Language.LSP.Protocol.Lens (HasContentChanges (..), HasEnd (..), HasParams (..), HasRange (..), HasStart (..), HasText (..), HasTextDocument (..), HasUri (..))
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types (ClientCapabilities, TextDocumentContentChangeEvent (..), TextDocumentContentChangePartial, toNormalizedUri, type (|?) (..))
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS (file_text, file_version)
import TreeSitter qualified as TS

handlers ::
  LogAction LSPTC (WithSeverity Text) ->
  ClientCapabilities ->
  LSP.Handlers LSPTC
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
  initializedHandler :: LSP.Handlers LSPTC
  initializedHandler =
    LSP.notificationHandler SMethod_Initialized $ \_notification -> do
      logger <& WithSeverity (T.pack "ClientCapabilities: " <> TL.toStrict (encodeToLazyText clientCapabilities)) Debug
      config <- LSP.getConfig
      logger <& WithSeverity (T.pack "Config: " <> TL.toStrict (encodeToLazyText config)) Debug
      pure ()

  textDocumentDidOpenHandler :: LSP.Handlers LSPTC
  textDocumentDidOpenHandler =
    LSP.notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
      let docUri = toNormalizedUri $ notification ^. params . textDocument . uri
      -- Try and get the virtual file for the uri:
      LSP.getVirtualFile docUri >>= \case
        -- If the virtual file does not exist...
        Nothing -> pure () -- TODO: report error
        Just docVirtualFile -> do
          let docRope = docVirtualFile ^. file_text
          success <- TC.documentOpen logger InputEncodingUTF8 docUri docRope
          when success $ do
            let docVersion = docVirtualFile ^. file_version
            logger <& T.pack ("Successfully opened " <> show docUri <> " version " <> show docVersion) `WithSeverity` Debug
            Gilear.publishParserDiagnostics logger docUri (Just 0)

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
      let docUri = toNormalizedUri $ notification ^. params . textDocument . uri
      let docChanges = notification ^. params . contentChanges
      -- Try and get the virtual file for the uri:
      LSP.getVirtualFile docUri >>= \case
        -- If the virtual file does not exist...
        Nothing -> pure () -- TODO: report error
        Just docVirtualFile -> do
          -- If there were any whole-document changes...
          let (docChangesPartial, docChangesWholeDocument) = partialDocumentContentChanges docChanges
          let docTextEdits = changePartialToTextEdit <$> docChangesPartial
          let docRope = docVirtualFile ^. file_text
          success <-
            if docChangesWholeDocument
              -- ... try and parse the whole document from the VFS
              then TC.documentChangeWholeDocument logger InputEncodingUTF8 docUri docRope
              -- Otherwise, parse the document incrementally...
              else TC.documentChangePartial logger InputEncodingUTF8 docUri docRope docTextEdits
          when success $ do
            let docVersion = docVirtualFile ^. file_version
            logger <& T.pack ("Successfully changed " <> show docUri <> " version " <> show docVersion) `WithSeverity` Debug
            Gilear.publishParserDiagnostics logger docUri (Just . fromIntegral $ docVersion)
            TC.lookupCache docUri >>= \case
              Nothing -> pure ()
              Just cacheItem -> do
                rootNode <- liftIO (TS.treeRootNode (TC.itemTree cacheItem))
                nodeText <- liftIO (T.decodeUtf8 <$> TS.showNode rootNode)
                logger <& nodeText `WithSeverity` Debug

  textDocumentDidCloseHandler :: LSP.Handlers LSPTC
  textDocumentDidCloseHandler =
    LSP.notificationHandler SMethod_TextDocumentDidClose $ \notification -> do
      let _uri = notification ^. params . textDocument . uri
      pure ()

  workspaceDidChangeConfiguration :: LSP.Handlers LSPTC
  workspaceDidChangeConfiguration =
    LSP.notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_notification ->
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
