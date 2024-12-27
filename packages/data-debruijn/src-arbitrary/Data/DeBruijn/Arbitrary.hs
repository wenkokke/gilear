{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeBruijn.Arbitrary (
  SomeSNat (..),
  SomePositiveSNat (..),
) where

import Data.DeBruijn (SomeIx (SomeIx), tabulate)
import GHC.TypeNats (SNat, withSomeSNat)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, elements)
import Test.QuickCheck.Modifiers (NonNegative (..), Positive (..))

instance Arbitrary SomeIx where
  arbitrary :: Gen SomeIx
  arbitrary = do
    SomePositiveSNat bound <- arbitrary
    elements [SomeIx bound value | value <- tabulate bound]

-- | A predicate for generating type-level numbers.
data SomeSNat = forall n. SomeSNat (SNat n)

deriving instance Show SomeSNat

instance Arbitrary SomeSNat where
  arbitrary :: Gen SomeSNat
  arbitrary = do
    n <- fromInteger . getNonNegative <$> arbitrary
    return $ withSomeSNat n SomeSNat

-- | A predicate for generating type-level positive numbers.
data SomePositiveSNat = forall n. SomePositiveSNat (SNat n)

deriving instance Show SomePositiveSNat

instance Arbitrary SomePositiveSNat where
  arbitrary :: Gen SomePositiveSNat
  arbitrary = do
    n <- fromInteger . getPositive <$> arbitrary
    return $ withSomeSNat n SomePositiveSNat
