{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Type.Nat.Singleton.Efficient.Arbitrary where

import Data.Type.Nat.Singleton.Efficient (SomeSNat (..), fromSomeSNat, toSomeSNat)
import Data.Word (Word16)
import Numeric.Natural (Natural)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..), arbitrarySizedNatural, coarbitraryIntegral, shrinkIntegral)
import Test.QuickCheck.Function (Function (..), functionIntegral, functionMap, (:->))
import Test.QuickCheck.Gen (Gen)

--------------------------------------------------------------------------------
-- QuickCheck instances for Natural
--------------------------------------------------------------------------------

instance Arbitrary Natural where
  arbitrary :: Gen Natural
  arbitrary = arbitrarySizedNatural

  shrink :: Natural -> [Natural]
  shrink = shrinkIntegral

instance CoArbitrary Natural where
  coarbitrary :: Natural -> Gen b -> Gen b
  coarbitrary = coarbitraryIntegral

instance Function Natural where
  function :: (Natural -> b) -> Natural :-> b
  function = functionIntegral

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeSNat
--------------------------------------------------------------------------------

instance Arbitrary SomeSNat where
  arbitrary :: Gen SomeSNat
  arbitrary = fmap (toSomeSNat @Word16) arbitrary

  shrink :: SomeSNat -> [SomeSNat]
  shrink = fmap (toSomeSNat @Word16) . shrinkIntegral . fromSomeSNat

instance CoArbitrary SomeSNat where
  coarbitrary :: SomeSNat -> Gen b -> Gen b
  coarbitrary = coarbitrary . (fromSomeSNat @Word16)

instance Function SomeSNat where
  function :: (SomeSNat -> b) -> SomeSNat :-> b
  function = functionMap (fromSomeSNat @Word16) toSomeSNat
