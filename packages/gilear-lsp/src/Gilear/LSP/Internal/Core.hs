{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.LSP.Internal.Core where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Aeson.Types (FromJSON (..), KeyValue ((.=)), Parser, Result (..), ToJSON (..), Value, fromJSON, object, withObject, (.!=), (.:?))
import Data.Default (Default (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Gilear.Internal.Core (TCEnv, TCIO, TCT (..), runTCIO, runTCT)
import Language.LSP.Protocol.Types (NormalizedUri)
import Language.LSP.Server (LanguageContextEnv, LspT (..), MonadLsp (..), runLspT)

--------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

--------------------------------------------------------------------------------

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
  { maxNumberOfProblems :: MaxNumberOfProblems
  }

defaultConfig :: Config
defaultConfig =
  Config
    { maxNumberOfProblems = 100
    }

instance Default Config where
  def = defaultConfig

instance FromJSON Config where
  parseJSON :: Value -> Parser Config
  parseJSON = withObject "Config" $ \l -> do
    maxNumberOfProblems <- l .:? "maxNumberOfProblems" .!= (def @Config).maxNumberOfProblems
    pure Config{..}

instance ToJSON Config where
  toJSON :: Config -> Value
  toJSON Config{..} =
    object
      [ "maxNumberOfProblems" .= maxNumberOfProblems
      ]

parseConfig :: Config -> Value -> Either Text Config
parseConfig _oldConfig newConfigRaw =
  case fromJSON newConfigRaw of
    Error errorMessage -> Left (T.pack errorMessage)
    Success newConfig -> Right newConfig

onConfigChange :: Config -> LSPTC ()
onConfigChange _newConfig = pure ()

--------------------------------------------------------------------------------
-- Utility Type Definitions
--------------------------------------------------------------------------------

type DiagnosticSource = Maybe Text

type MaxNumberOfProblems = Int
