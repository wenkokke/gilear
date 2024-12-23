{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Gilear.Internal.Parser where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Gilear.Internal.Core (MonadTC, modifyTreeCache, withParser)
import Gilear.Internal.Parser.TreeCache qualified as TreeCache
import Language.LSP.Protocol.Types (NormalizedUri)
import TreeSitter qualified as TS

-- | Parse an entire file.
parse :: (MonadTC m) => NormalizedUri -> Text -> m (Maybe TS.Tree)
parse uri text =
  withParser $ \parser -> do
    maybeTree <- liftIO $ TS.parserParseByteStringWithEncoding parser Nothing (T.encodeUtf8 text) TS.InputEncodingUTF8
    case maybeTree of
      Nothing -> do
        -- TODO: Handle parse failure properly.
        error $ "Parsing " <> show uri <> " failed"
      Just tree -> do
        modifyTreeCache $ \treeCache ->
          (TreeCache.insert uri tree treeCache, Just tree)
