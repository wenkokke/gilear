{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.Internal.Core where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Text qualified as T
import Gilear.Internal.Parser.Cache (Cache, CacheItem)
import Gilear.Internal.Parser.Cache qualified as Cache
import Gilear.Internal.Parser.Core (Parser)
import Gilear.Internal.Parser.Core qualified as Parser

--------------------------------------------------------------------------------
-- Executable Name
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
type TCEnv :: Type -> Type
data TCEnv uri = TCEnv
  { parser :: Parser
  , cacheVar :: IORef (Cache uri)
  }

-- | Create an empty type-checker environment.
newTCEnv :: IO (TCEnv uri)
newTCEnv = do
  parser <- Parser.new
  cacheVar <- newIORef Cache.empty
  pure $ TCEnv{..}

--------------------------------------------------------------------------------
-- Type-Checker Monad Class
--------------------------------------------------------------------------------

type MonadTC :: Type -> (Type -> Type) -> Constraint
type MonadTC uri m = (MonadIO m, MonadReader (TCEnv uri) m)

-- | Get the type-checker environment.
askTCEnv :: (MonadTC uri m) => m (TCEnv uri)
askTCEnv = ask

-- * Parser

-- | Get the `Parser`.
askParser :: (MonadTC uri m) => m Parser
askParser = (.parser) <$> askTCEnv

-- | Run a type-checking action with the `Parser`.
withParser :: (MonadTC uri m) => (Parser -> m a) -> m a
withParser action =
  askParser >>= action

-- * Cache

-- | Get the `Cache`.
askCache :: (MonadTC uri m) => m (Cache uri)
askCache = askTCEnv >>= liftIO . readIORef . (.cacheVar)

-- | Get the parse for a file.
lookupCache :: (Hashable uri, MonadTC uri m) => uri -> m (Maybe CacheItem)
lookupCache uri = askCache <&> Cache.lookup uri

-- | Modify the `Cache`.
modifyCache :: (MonadTC uri m) => (Cache uri -> (Cache uri, b)) -> m b
modifyCache f = askTCEnv >>= liftIO . (`atomicModifyIORef'` f) . (.cacheVar)

-- | Variant of `modifyCache` that does not return a result.
modifyCache_ :: (MonadTC uri m) => (Cache uri -> Cache uri) -> m ()
modifyCache_ f = modifyCache ((,()) . f)

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
newtype TCIO a = TCIO {unTCIO :: ResourceT IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

-- | Run the type-checker IO monad.
runTCIO :: TCIO a -> IO a
runTCIO action = runResourceT (unTCIO action)

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
