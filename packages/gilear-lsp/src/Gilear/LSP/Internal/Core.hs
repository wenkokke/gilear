{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.LSP.Internal.Core where

import Colog.Core (LogAction, WithSeverity)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Aeson.Types (FromJSON (..), KeyValue ((.=)), Parser, Result (..), ToJSON (..), Value, fromJSON, object, withObject, (.!=), (.:?))
import Data.Default (Default (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Mixed.Rope (Rope)
import GHC.Exts (oneShot)
import Gilear.Internal.Core (MonadTc (..), TcEnv)
import Gilear.Internal.Parser qualified as Tc
import Gilear.Internal.Parser.TextEdit (TextEdit)
import Language.LSP.Protocol.Types (NormalizedUri)
import Language.LSP.Server (LanguageContextEnv, MonadLsp (..))

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
--
-- The `LspTc` monad uses a one-shot reader encoding. For details, see the GHC
-- Note [The one-shot state monad trick] or https://github.com/ghc/ghc/blob/
-- ab3ab3e3d489a351e84f4fe681de1731549376a2/compiler/GHC/Utils/Monad.hs#L259
--------------------------------------------------------------------------------

type LspTc :: Type -> Type
newtype LspTc a = LspTc' (LanguageContextEnv Config -> TcEnv NormalizedUri -> IO a)

-- deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (TcEnv NormalizedUri))

pattern LspTc :: forall a. (LanguageContextEnv Config -> TcEnv NormalizedUri -> IO a) -> LspTc a
pattern LspTc m <- LspTc' m
  where
    LspTc m = LspTc' (oneShot m)

{-# COMPLETE LspTc #-}

runLspTc :: LanguageContextEnv Config -> TcEnv NormalizedUri -> LspTc a -> IO a
runLspTc lcEnv tcEnv (LspTc f) = f lcEnv tcEnv

instance Functor LspTc where
  fmap :: (a -> b) -> LspTc a -> LspTc b
  fmap f (LspTc ma) = LspTc $ \lcEnv tcEnv -> f <$> ma lcEnv tcEnv

instance Applicative LspTc where
  pure :: a -> LspTc a
  pure x = LspTc $ \_lcEnv _tcEnv -> pure x

  (<*>) :: LspTc (a -> b) -> LspTc a -> LspTc b
  LspTc mf <*> LspTc ma = LspTc $ \lcEnv tcEnv -> mf lcEnv tcEnv <*> ma lcEnv tcEnv

instance Monad LspTc where
  (>>=) :: LspTc a -> (a -> LspTc b) -> LspTc b
  LspTc ma >>= mf =
    LspTc $ \lcEnv tcEnv ->
      ma lcEnv tcEnv
        >>= \a -> let LspTc b = mf a in b lcEnv tcEnv

instance MonadIO LspTc where
  liftIO :: IO a -> LspTc a
  liftIO m = LspTc $ \_lcEnv _tcEnv -> m

instance MonadUnliftIO LspTc where
  withRunInIO :: ((forall a. LspTc a -> IO a) -> IO b) -> LspTc b
  withRunInIO k = LspTc $ \lcEnv tcEnv -> k (runLspTc lcEnv tcEnv)

instance MonadTc NormalizedUri LspTc where
  getTcEnv :: LspTc (TcEnv NormalizedUri)
  getTcEnv = LspTc $ \_lcEnv tcEnv -> pure tcEnv

instance MonadLsp Config LspTc where
  getLspEnv :: LspTc (LanguageContextEnv Config)
  getLspEnv = LspTc $ \lcEnv _tcEnv -> pure lcEnv

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

onConfigChange :: Config -> LspTc ()
onConfigChange _newConfig = pure ()

--------------------------------------------------------------------------------
-- Utility Type Definitions
--------------------------------------------------------------------------------

type DiagnosticSource = Maybe Text

type MaxNumberOfProblems = Int

--------------------------------------------------------------------------------
-- Specialise Gilear compiler API to LspTc
--------------------------------------------------------------------------------

{-# SPECIALIZE Tc.documentOpen ::
  LogAction LspTc (WithSeverity Text) ->
  Tc.InputEncoding ->
  NormalizedUri ->
  Rope ->
  LspTc Bool
  #-}

{-# SPECIALIZE Tc.documentChangeWholeDocument ::
  LogAction LspTc (WithSeverity Text) ->
  Tc.InputEncoding ->
  NormalizedUri ->
  Rope ->
  LspTc Bool
  #-}

{-# SPECIALIZE Tc.documentChangePartial ::
  LogAction LspTc (WithSeverity Text) ->
  Tc.InputEncoding ->
  NormalizedUri ->
  Rope ->
  [TextEdit] ->
  LspTc Bool
  #-}
