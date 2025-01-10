{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.Internal.Core where

import Colog.Core (LogAction, Severity (..), (<&))
import Colog.Core.Severity (WithSeverity (..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
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
type TCEnv :: Type -> Type
newtype TCEnv uri = TCEnv
  { parserEnv :: ParserEnv uri
  }

-- | Create an empty type-checker environment.
newTCEnv :: IO (TCEnv uri)
newTCEnv = do
  parserEnv <- newParserEnv
  pure $ TCEnv{..}

--------------------------------------------------------------------------------
-- Type-Checker Monad Class
--------------------------------------------------------------------------------

type MonadTC :: Type -> (Type -> Type) -> Constraint
type MonadTC uri m = (Show uri, Hashable uri, MonadIO m, MonadReader (TCEnv uri) m)

-- | Get the type-checker environment.
askTCEnv :: (MonadTC uri m) => m (TCEnv uri)
askTCEnv = ask

-- | Get the type-checker environment.
askParserEnv :: (MonadTC uri m) => m (ParserEnv uri)
askParserEnv = (.parserEnv) <$> askTCEnv

-- | Get the `Parser`.
askParser :: (MonadTC uri m) => m Parser
askParser = liftIO . readIORef . (.parserVar) =<< askParserEnv

-- | Run a type-checking action with the `Parser`.
withParser :: (MonadTC uri m) => (Parser -> m a) -> m a
withParser action = action =<< askParser

-- | Get the `Language`.
askLanguage :: (MonadTC uri m) => m Language
askLanguage = liftIO . TS.parserLanguage =<< askParser

-- | Run a type-checking action with the `Language`.
withLanguage :: (MonadTC uri m) => (Language -> m a) -> m a
withLanguage action = action =<< askLanguage

-- -- * Cache

-- | Get the `ParserCache`.
askParserCache :: (MonadTC uri m) => m (ParserCache uri)
askParserCache = liftIO . readIORef . (.parserCacheVar) =<< askParserEnv

-- | Get the parse for a file.
lookupCache :: (Hashable uri, MonadTC uri m) => uri -> m (Maybe ParserCacheItem)
lookupCache uri = Cache.lookup uri <$> askParserCache

-- | Modify the `Cache`.
modifyCache :: (MonadTC uri m) => (ParserCache uri -> (ParserCache uri, b)) -> m b
modifyCache f = liftIO . (`atomicModifyIORef'` f) . (.parserCacheVar) =<< askParserEnv

-- | Variant of `modifyCache` that does not return a result.
modifyCache_ :: (MonadTC uri m) => (ParserCache uri -> ParserCache uri) -> m ()
modifyCache_ f = modifyCache ((,()) . f)

-- | Assert that there is cache item associated with the given @uri@.
assertNoCacheItem ::
  (MonadTC uri m) =>
  LogAction m (WithSeverity Text) ->
  uri ->
  m ()
assertNoCacheItem logger uri = do
  maybeCacheItem <- lookupCache uri
  when (isJust maybeCacheItem) $ do
    let message = T.pack $ printf "found cache item for %s" (show uri)
    logger <& WithSeverity message Warning

-- * Diagnostics

askDiagnostics :: (MonadTC uri m) => uri -> m (Maybe Diagnostics)
askDiagnostics = fmap (fmap (.itemDiagnostics)) . lookupCache

insertDiagnostics :: (MonadTC uri m) => uri -> Diagnostics -> m ()
insertDiagnostics uri newDiagnostics = modifyCache_ . flip Cache.adjust uri $ \Cache.ParserCacheItem{..} ->
  Cache.ParserCacheItem{itemDiagnostics = itemDiagnostics <> newDiagnostics, ..}

--------------------------------------------------------------------------------
-- Type-Checker Monad Stack
--------------------------------------------------------------------------------

{-| Type-Checker IO Monad.

  The base of the type-checker monad stack is 'IO'. This is for two reasons:

  1. The tree-sitter parser---particularly its error correction---is not
     guaranteed to be deterministic and cannot be used from a pure function.
  2. The type-checker environment, which contains mutable shared state, is
     threaded through the type-checker concurrently rather than immutable
     state being threaded through sequentially.
-}
type TCIO :: Type -> Type
newtype TCIO a = TCIO {unTCIO :: IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

-- | Run the type-checker IO monad.
runTCIO :: TCIO a -> IO a
runTCIO = unTCIO

type TCT :: Type -> (Type -> Type) -> Type -> Type
newtype TCT uri m a = TCT {unTCT :: ReaderT (TCEnv uri) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (TCEnv uri))

deriving newtype instance (MonadIO m) => MonadIO (TCT uri m)

deriving newtype instance (MonadUnliftIO m) => MonadUnliftIO (TCT uri m)

type TC :: Type -> Type -> Type
newtype TC uri a = TC {unTC :: TCT uri TCIO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (TCEnv uri))

-- | Run the type-checker monad.
runTCT :: TCEnv uri -> TCT uri m a -> m a
runTCT env action = runReaderT (unTCT action) env
