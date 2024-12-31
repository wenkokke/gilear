{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Gilear.LSP.Internal.Handlers where

import Colog (Severity (..))
import Colog.Core (LogAction, WithSeverity (WithSeverity), (<&))
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Writer (MonadWriter (..), Writer, runWriter)
import Data.Bifunctor (Bifunctor (..))
import Data.Monoid (Any (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Lines qualified as Rope
import Gilear.Internal.Core (lookupCache)
import Gilear.Internal.Parser (InputEncoding (..), TextEdit (..))
import Gilear.Internal.Parser qualified as TC
import Gilear.Internal.Parser.Cache (CacheItem (..))
import Gilear.LSP.Internal.Core (LSPTC)
import Language.LSP.Protocol.Lens (HasContentChanges (..), HasEnd (..), HasParams (..), HasRange (..), HasStart (..), HasText (..), HasTextDocument (..), HasUri (..))
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types (ClientCapabilities, TextDocumentContentChangeEvent (..), TextDocumentContentChangePartial, toNormalizedUri, type (|?) (..))
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS (file_text)
import TreeSitter qualified as TS

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
      let docUri = toNormalizedUri $ notification ^. params . textDocument . uri
      -- Try and get the virtual file for the uri:
      LSP.getVirtualFile docUri >>= \case
        -- If the virtual file does not exist...
        Nothing -> pure () -- TODO: report error
        Just docVirtualFile -> do
          let docRope = docVirtualFile ^. file_text
          _success <- TC.documentOpen logger InputEncodingUTF8 docUri docRope
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
          if docChangesWholeDocument
            -- ... try and parse the whole document from the VFS
            then void $ TC.documentChangeWholeDocument logger InputEncodingUTF8 docUri docRope
            -- Otherwise, parse the document incrementally...
            else void $ TC.documentChangePartial logger InputEncodingUTF8 docUri docRope docTextEdits
      -- Log the tree for debugging purposes
      lookupCache docUri >>= \case
        Nothing -> pure ()
        Just (CacheItem _docRope docTree) -> do
          docRootNode <- liftIO $ TS.treeRootNode docTree
          docTreeString <- liftIO $ TS.showNode docRootNode
          let message = T.decodeUtf8 docTreeString
          logger <& WithSeverity message Debug

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
    { textEditStartPosition = positionToPosition $ changePartial ^. range . start
    , textEditOldEndPosition = positionToPosition $ changePartial ^. range . end
    , textEditNewText = changePartial ^. text
    }

positionToPosition :: LSP.Position -> Rope.Position
positionToPosition (LSP.Position row column) =
  Rope.Position (fromIntegral row) (fromIntegral column)

{-| Get all 'TextDocumentContentChangePartial' items. The 'Bool' value is
    'True' if any of the 'TextDocumentContentChangeEvent' items were
    'TextDocumentContentChangeWholeDocument' items.
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
