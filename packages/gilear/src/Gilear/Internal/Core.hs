{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.Internal.Core where

import Colog.Core (LogAction, Severity (..), (<&))
import Colog.Core.Severity (WithSeverity (..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Hashable (Hashable)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Gilear.Internal.Core.Diagnostics (Diagnostics)
import Gilear.Internal.Parser.Cache (ParserCache, ParserCacheItem)
import Gilear.Internal.Parser.Cache qualified as Cache
import Gilear.Internal.Parser.Core (ParserEnv (..), newParserEnv)
import Text.Printf (printf)
import TreeSitter (Language, Parser)
import TreeSitter qualified as TS

--------------------------------------------------------------------------------
-- Package Name
--------------------------------------------------------------------------------

packageName :: Text
packageName = T.pack "gilear"

--------------------------------------------------------------------------------
-- Type-Checker Environments
--------------------------------------------------------------------------------

{-| Type-Checker Environment.

  This is an environment, rather than state, because it is intended to be
  used with the reader monad, rather than the state monad. However, it is
  intended to hold references to mutable state.
-}

-- TODO: use `MVar` or `TMVar` for the parser to avoid concurrent use
type TcEnv :: Type -> Type
newtype TcEnv uri = TcEnv
  { parserEnv :: ParserEnv uri
  }

-- | Create an empty type-checker environment.
newTcEnv :: IO (TcEnv uri)
newTcEnv = do
  parserEnv <- newParserEnv
  pure $ TcEnv{..}

--------------------------------------------------------------------------------
-- Type-Checker Monad Class
--------------------------------------------------------------------------------

type MonadTc :: Type -> (Type -> Type) -> Constraint
class (Show uri, Hashable uri, MonadUnliftIO m) => MonadTc uri m | m -> uri where
  -- | Get the type-checker environment.
  getTcEnv :: (MonadTc uri m) => m (TcEnv uri)

-- | Get the type-checker environment.
getParserEnv :: (MonadTc uri m) => m (ParserEnv uri)
getParserEnv = (.parserEnv) <$> getTcEnv
{-# INLINEABLE getParserEnv #-}

-- | Get the `Parser`.
getParser :: (MonadTc uri m) => m Parser
getParser = liftIO . readIORef . (.parserVar) =<< getParserEnv
{-# INLINEABLE getParser #-}

-- | Run a type-checking action with the `Parser`.
withParser :: (MonadTc uri m) => (Parser -> m a) -> m a
withParser action = action =<< getParser
{-# INLINEABLE withParser #-}

-- | Get the `Language`.
getLanguage :: (MonadTc uri m) => m Language
getLanguage = liftIO . TS.parserLanguage =<< getParser
{-# INLINEABLE getLanguage #-}

-- | Run a type-checking action with the `Language`.
withLanguage :: (MonadTc uri m) => (Language -> m a) -> m a
withLanguage action = action =<< getLanguage
{-# INLINEABLE withLanguage #-}

-- -- * Cache

-- | Get the `ParserCache`.
getParserCache :: (MonadTc uri m) => m (ParserCache uri)
getParserCache = liftIO . readIORef . (.parserCacheVar) =<< getParserEnv
{-# INLINEABLE getParserCache #-}

-- | Get the parse for a file.
lookupCache :: (Hashable uri, MonadTc uri m) => uri -> m (Maybe ParserCacheItem)
lookupCache uri = Cache.lookup uri <$> getParserCache
{-# INLINEABLE lookupCache #-}

-- | Modify the `Cache`.
modifyCache :: (MonadTc uri m) => (ParserCache uri -> (ParserCache uri, b)) -> m b
modifyCache f = liftIO . (`atomicModifyIORef'` f) . (.parserCacheVar) =<< getParserEnv
{-# INLINEABLE modifyCache #-}

-- | Variant of `modifyCache` that does not return a result.
modifyCache_ :: (MonadTc uri m) => (ParserCache uri -> ParserCache uri) -> m ()
modifyCache_ f = modifyCache ((,()) . f)
{-# INLINEABLE modifyCache_ #-}

-- | Assert that there is cache item associated with the given @uri@.
assertNoCacheItem ::
  (MonadTc uri m) =>
  LogAction m (WithSeverity Text) ->
  uri ->
  m ()
assertNoCacheItem logger uri = do
  maybeCacheItem <- lookupCache uri
  when (isJust maybeCacheItem) $ do
    let message = T.pack $ printf "found cache item for %s" (show uri)
    logger <& WithSeverity message Warning
{-# INLINEABLE assertNoCacheItem #-}

-- * Diagnostics

getDiagnostics :: (MonadTc uri m) => uri -> m (Maybe Diagnostics)
getDiagnostics = fmap (fmap (.itemDiag)) . lookupCache
{-# INLINEABLE getDiagnostics #-}

insertDiagnostics :: (MonadTc uri m) => uri -> Diagnostics -> m ()
insertDiagnostics uri newDiag = modifyCache_ . flip Cache.adjust uri $ \Cache.ParserCacheItem{itemDiag = oldDiag, ..} ->
  Cache.ParserCacheItem{itemDiag = oldDiag <> newDiag, ..}
{-# INLINEABLE insertDiagnostics #-}
