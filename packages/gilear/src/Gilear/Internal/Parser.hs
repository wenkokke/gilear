{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Gilear.Internal.Parser where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Gilear.Internal.Core (modifyTreeCache, withParser, MonadTC)
import Gilear.Internal.Parser.TreeCache qualified as TreeCache
import Language.LSP.Protocol.Types (NormalizedUri)
import TreeSitter qualified as TS

-- | Parse an entire file.
parse :: MonadTC m => NormalizedUri -> Text -> m (Maybe TS.Tree)
parse uri text = do
  parseText text >>= \case
    Nothing -> parseFailure uri >> pure Nothing
    Just tree -> modifyTreeCache $ \treeCache ->
      (TreeCache.insert uri tree treeCache, Just tree)

-- | Internal helper: Parse `Text` using the `TS.Parser` in `TC`.
parseText :: MonadTC m => Text -> m (Maybe TS.Tree)
parseText text =
  withParser $ \parser ->
    liftIO $
      TS.parserParseByteStringWithEncoding
        parser
        Nothing
        (T.encodeUtf8 text)
        TS.InputEncodingUTF8

{-| Internal helper: Handle parse failure.

    See `TS.parserParse` for conditions under which failure occurs.

    TODO: Handle parse failure properly.
-}
parseFailure :: MonadTC m => NormalizedUri -> m ()
parseFailure uri =
  error $ "Parsing " <> show uri <> " failed"
