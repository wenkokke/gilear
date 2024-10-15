{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear where

import Control.Concurrent.Class.MonadSTM (MonadSTM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Kind (Type)

--------------------------------------------------------------------------------
-- Type-Checker Monad
--------------------------------------------------------------------------------

{- | Type-Checker Environment.

  This is an environment, rather than state, because it is intended to be
  used with the reader monad, rather than the state monad. However, it is
  intended to hold references to mutable state ('TVar').
-}
type TCEnv :: Type
data TCEnv = TCEnv

-- | Create an empty type-checker environment.
newTCEnv :: IO TCEnv
newTCEnv = pure TCEnv

{- | Type-Checker Monad.

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

-- | Run the type-checker monad.
runTC :: TCEnv -> TC a -> IO a
runTC env action = runResourceT (runReaderT (unTC action) env)
