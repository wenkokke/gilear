{-# LANGUAGE LambdaCase #-}

module Gilear.Internal.Parser where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Gilear.Internal.Core (TC, modifyTreeCache, withParser)
import Gilear.Internal.Parser.TreeCache qualified as TreeCache
import Language.LSP.Protocol.Types (NormalizedUri)
import TreeSitter qualified as TS

-- | Parse an entire file.
parse :: NormalizedUri -> Text -> TC (Maybe TS.Tree)
parse uri text = do
  parseText text >>= \case
    Nothing -> parseFailure uri >> pure Nothing
    Just tree -> modifyTreeCache $ \treeCache ->
      (TreeCache.insert uri tree treeCache, Just tree)

-- | Internal helper: Parse `Text` using the `TS.Parser` in `TC`.
parseText :: Text -> TC (Maybe TS.Tree)
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
parseFailure :: NormalizedUri -> TC ()
parseFailure uri =
  error $ "Parsing " <> show uri <> " failed"