{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.LSP.Internal.Core where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Aeson.Types (Value)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Gilear.Internal.Core (TCEnv, TCIO, TCT (..), runTCIO, runTCT)
import Language.LSP.Protocol.Types (NormalizedUri)
import Language.LSP.Server (LanguageContextEnv, LspT (..), MonadLsp (..), runLspT)

--------------------------------------------------------------------------------
-- Executable Name
--------------------------------------------------------------------------------

packageName :: Text
packageName = T.pack "gilear-lsp"

--------------------------------------------------------------------------------
-- Language-Server Type-Checker Monad Stack
--------------------------------------------------------------------------------

type LSPTC :: Type -> Type
newtype LSPTC a = LSPTC {unLSPTC :: TCT NormalizedUri (LspT Config TCIO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (TCEnv NormalizedUri))

instance MonadLsp Config LSPTC where
  getLspEnv :: LSPTC (LanguageContextEnv Config)
  getLspEnv = LSPTC (TCT getLspEnv)

runLSPTC :: LanguageContextEnv Config -> TCEnv NormalizedUri -> LSPTC a -> IO a
runLSPTC languageContextEnv tcEnv action =
  runTCIO (runLspT languageContextEnv (runTCT tcEnv (unLSPTC action)))

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
