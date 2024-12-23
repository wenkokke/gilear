{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.Internal.Core where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Gilear.Internal.Parser.Core (Parser)
import Gilear.Internal.Parser.Core qualified as Parser
import Gilear.Internal.Parser.TreeCache (TreeCache)
import Gilear.Internal.Parser.TreeCache qualified as TreeCache
import Language.LSP.Protocol.Types (NormalizedUri)
import TreeSitter qualified as TS
import qualified Control.Monad.Accum as TreeCache
import Data.Functor ((<&>))

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

{-| Type-Checker Monad.

  The base of the type-checker monad stack is 'IO'. This is for two reasons:

  1. The tree-sitter parser---particularly its error correction---is not
     guaranteed to be deterministic and cannot be used from a pure function.
  2. The type-checker environment, which contains mutable shared state, is
     threaded through the type-checker concurrently rather than immutable
     state being threaded through sequentially.
-}
type TC :: Type -> Type
newtype TC a = TC {unTC :: ReaderT TCEnv (ResourceT IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

-- | Get the type-checker environment.
askTCEnv :: TC TCEnv
askTCEnv = TC ask

-- | Get the `Parser`.
askParser :: TC Parser
askParser = (.parser) <$> askTCEnv

-- | Run a type-checking action with the `Parser`.
withParser :: (Parser -> TC a) -> TC a
withParser action = askParser >>= action

-- | Get the `TreeCache`.
askTreeCache :: TC TreeCache
askTreeCache =
  askTCEnv >>= liftIO . readIORef . (.treeCacheVar)

-- | Modify the `TreeCache`.
modifyTreeCache :: (TreeCache -> (TreeCache, b)) -> TC b
modifyTreeCache f =
  askTCEnv >>= liftIO . (`atomicModifyIORef'` f) . (.treeCacheVar)

-- | Get the parse for a file.
askTree :: NormalizedUri -> TC (Maybe TS.Tree)
askTree uri =
  askTreeCache <&> TreeCache.lookup uri

-- | Run the type-checker monad.
runTC :: TCEnv -> TC a -> IO a
runTC env action = runResourceT (runReaderT (unTC action) env)
