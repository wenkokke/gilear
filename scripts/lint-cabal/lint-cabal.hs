{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.Writer (MonadIO (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.PackageDescription (PackageDescription (..), PackageIdentifier (..))
import Data.Foldable (for_)
import System.FilePath.Glob (glob)
import Lint.Cabal (runLint, readGenericPackageDescription)

main :: IO ()
main = void . runLint $ do
  cabalFiles <- liftIO $ glob "./**/*.cabal"
  for_ cabalFiles $ \cabalFile -> do
    pkgDesc <- readGenericPackageDescription cabalFile
    liftIO . print $ pkgDesc.packageDescription.package.pkgName
    for_ pkgDesc.condSubLibraries $ \(subLibName, _subLib) -> do
      liftIO . print $ subLibName
    for_ pkgDesc.condTestSuites $ \(testSuiteName, _testSuite) -> do
      liftIO . print $ testSuiteName

--------------------------------------------------------------------------------
-- Lints
--------------------------------------------------------------------------------

