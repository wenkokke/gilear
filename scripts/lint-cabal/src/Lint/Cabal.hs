{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lint.Cabal where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus (..), unless)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer (MonadIO (..), MonadWriter (..), WriterT (..))
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (..), for_)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (PError, PWarning)
import Distribution.Parsec.Error (showPError)
import Distribution.Parsec.Warning (showPWarning)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import System.Exit (exitFailure)

--------------------------------------------------------------------------------
-- Lint Monad
--------------------------------------------------------------------------------

newtype Lint a = Lint (MaybeT (WriterT [Issue] IO) a)

deriving newtype instance Functor Lint

deriving newtype instance Applicative Lint

deriving newtype instance Monad Lint

deriving newtype instance Alternative Lint

deriving newtype instance MonadPlus Lint

deriving newtype instance MonadIO Lint

runLint :: Lint a -> IO (Maybe a)
runLint (Lint linter) = do
  (result, issues) <- runWriterT (runMaybeT linter)
  unless (null issues) $ for_ issues (putStrLn . showIssue) >> exitFailure
  pure result

reportAll :: [Issue] -> Lint ()
reportAll = Lint . tell

report :: Issue -> Lint ()
report = reportAll . (: [])

throwAll :: [Issue] -> Lint a
throwAll issues = reportAll issues >> mzero

readGenericPackageDescription :: FilePath -> Lint GenericPackageDescription
readGenericPackageDescription cabalFile = do
  (pWarnings, packageDescriptionOrPErrors) <-
    runParseResult . parseGenericPackageDescription <$> liftIO (BS.readFile cabalFile)
  reportAll (PWarning cabalFile <$> pWarnings)
  let handleError = throwAll . fmap (PError cabalFile) . toList . snd
  either handleError pure packageDescriptionOrPErrors

--------------------------------------------------------------------------------
-- Issue
--------------------------------------------------------------------------------

data Issue
  = PWarning FilePath PWarning
  | PError FilePath PError

showIssue :: Issue -> String
showIssue = \case
  PWarning cabalFile pWarning -> showPWarning cabalFile pWarning
  PError cabalFile pError -> showPError cabalFile pError
