{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.LSP.Internal.Core where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Aeson.Types (Value)
import Data.Kind (Type)
import Data.Text (Text)
import Gilear (TC, TCEnv, runTC)
import Language.LSP.Server (LanguageContextEnv, LspT, MonadLsp, runLspT)

--------------------------------------------------------------------------------
-- Language-Server Type-Checker Monad Stack
--------------------------------------------------------------------------------

type LSPTC :: Type -> Type
newtype LSPTC a = LSPTC {unLSPTC :: LspT Config TC a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadLsp Config)

runLSPTC :: LanguageContextEnv Config -> TCEnv -> LSPTC a -> IO a
runLSPTC languageContextEnv tcEnv =
  runTC tcEnv . runLspT languageContextEnv . unLSPTC

--------------------------------------------------------------------------------
-- Language-Server Configuration
--------------------------------------------------------------------------------

type Config :: Type
data Config = Config

parseConfig :: Config -> Value -> Either Text Config
parseConfig _oldConfig _newConfig = Right Config

defaultConfig :: Config
defaultConfig = Config

onConfigChange :: Config -> LSPTC ()
onConfigChange _newConfig = pure ()
