{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeBruijn.Index.Inductive.Arbitrary (
  arbitraryIx,
) where

import Data.DeBruijn.Index.Inductive (Ix (..), SomeIx (..), toSomeIx)
import Data.Type.Nat (Nat (..))
import Data.Type.Nat.Singleton.Inductive (SNat (..))
import Numeric.Natural (Natural)
import Numeric.Natural.Arbitrary ()
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, oneof)
import Test.QuickCheck.Modifiers (NonNegative (..), Positive (..))

instance Arbitrary SomeIx where
  arbitrary :: Gen SomeIx
  arbitrary = do
    Positive boundOverIndex <- arbitrary
    NonNegative index <- arbitrary
    pure $ toSomeIx @Natural (index + boundOverIndex, index)

instance Arbitrary (Ix (S Z)) where
  arbitrary :: Gen (Ix (S Z))
  arbitrary = pure FZ

instance (forall m. Arbitrary (Ix (S m))) => Arbitrary (Ix (S (S n))) where
  arbitrary :: Gen (Ix (S (S n)))
  arbitrary = oneof [pure FZ, FS <$> arbitrary]

arbitraryIx :: SNat (S n) -> Gen (Ix (S n))
arbitraryIx (S Z) = pure FZ
arbitraryIx (S n@(S _)) = oneof [pure FZ, FS <$> arbitraryIx n]
