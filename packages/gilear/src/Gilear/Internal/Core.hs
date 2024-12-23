{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Gilear.Internal.Core where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Constraint, Type)
import Gilear.Internal.Parser.Core (Parser)
import Gilear.Internal.Parser.Core qualified as Parser
import Gilear.Internal.Parser.TreeCache (TreeCache)
import Gilear.Internal.Parser.TreeCache qualified as TreeCache
import Language.LSP.Protocol.Types (NormalizedUri)
import TreeSitter qualified as TS

--------------------------------------------------------------------------------
-- Type-Checker Environments
--------------------------------------------------------------------------------

{-| Type-Checker Environment.

  This is an environment, rather than state, because it is intended to be
  used with the reader monad, rather than the state monad. However, it is
  intended to hold references to mutable state.
-}
type TCEnv :: Type
data TCEnv = TCEnv
  { parser :: Parser
  , treeCacheVar :: IORef TreeCache
  }

-- | Create an empty type-checker environment.
newTCEnv :: IO TCEnv
newTCEnv = do
  parser <- Parser.new
  treeCacheVar <- newIORef TreeCache.empty
  pure $ TCEnv{..}

--------------------------------------------------------------------------------
-- Type-Checker Monad Class
--------------------------------------------------------------------------------

type MonadTC :: (Type -> Type) -> Constraint
type MonadTC m = (MonadIO m, MonadReader TCEnv m)

-- | Get the type-checker environment.
askTCEnv :: (MonadTC m) => m TCEnv
askTCEnv = ask

-- | Get the `Parser`.
askParser :: (MonadTC m) => m Parser
askParser = (.parser) <$> askTCEnv

-- | Run a type-checking action with the `Parser`.
withParser :: (MonadTC m) => (Parser -> m a) -> m a
withParser action =
  askParser >>= action

-- | Get the `TreeCache`.
askTreeCache :: (MonadTC m) => m TreeCache
askTreeCache =
  askTCEnv >>= liftIO . readIORef . (.treeCacheVar)

-- | Modify the `TreeCache`.
modifyTreeCache :: (MonadTC m) => (TreeCache -> (TreeCache, b)) -> m b
modifyTreeCache f =
  askTCEnv >>= liftIO . (`atomicModifyIORef'` f) . (.treeCacheVar)

-- | Get the parse for a file.
askTree :: (MonadTC m) => NormalizedUri -> m (Maybe TS.Tree)
askTree uri =
  askTreeCache <&> TreeCache.lookup uri

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

type TCT :: (Type -> Type) -> Type -> Type
newtype TCT m a = TCT {unTCT :: ReaderT TCEnv m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader TCEnv)

deriving newtype instance MonadIO m => MonadIO (TCT m)
deriving newtype instance MonadUnliftIO m => MonadUnliftIO (TCT m)

type TC :: Type -> Type
newtype TC a = TC {unTC :: TCT TCIO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader TCEnv)

-- | Run the type-checker monad.
runTCT :: TCEnv -> TCT m a -> m a
runTCT env action = runReaderT (unTCT action) env

