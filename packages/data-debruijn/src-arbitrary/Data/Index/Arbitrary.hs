{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Index.Arbitrary (
  -- arbitraryIx,
) where

import Data.Index (Ix (..), SomeIx (..), toSomeIxRaw)
import Data.Type.Nat (Nat (..))
import Data.Type.Nat.Singleton
import Data.Type.Nat.Singleton.Arbitrary ()
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, oneof)
import Test.QuickCheck.Modifiers (NonNegative (..), Positive (..))

instance Arbitrary SomeIx where
  arbitrary :: Gen SomeIx
  arbitrary = do
    Positive boundOverIndex <- arbitrary
    NonNegative index <- arbitrary
    pure $ toSomeIxRaw (index + boundOverIndex, index)

instance Arbitrary (Ix (S Z)) where
  arbitrary :: Gen (Ix (S Z))
  arbitrary = pure FZ

instance (forall m. Arbitrary (Ix (S m))) => Arbitrary (Ix (S (S n))) where
  arbitrary :: Gen (Ix (S (S n)))
  arbitrary = oneof [pure FZ, FS <$> arbitrary]

-- arbitraryIx :: SNat (S n) -> Gen (Ix (S n))
-- arbitraryIx (S Z) = pure FZ
-- arbitraryIx (S n) = oneof [pure FZ, FS <$> arbitrary]
