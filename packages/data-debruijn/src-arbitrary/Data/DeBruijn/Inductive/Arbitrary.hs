{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DeBruijn.Inductive.Arbitrary where

import Data.DeBruijn.Inductive (SomeSNat (..), fromSomeSNat, toSomeSNat)
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
  arbitrary = fmap toSomeSNat arbitrary

  shrink :: SomeSNat -> [SomeSNat]
  shrink = fmap toSomeSNat . shrinkIntegral . fromSomeSNat

instance CoArbitrary SomeSNat where
  coarbitrary :: SomeSNat -> Gen b -> Gen b
  coarbitrary = coarbitrary . fromSomeSNat

instance Function SomeSNat where
  function :: (SomeSNat -> b) -> SomeSNat :-> b
  function = functionMap fromSomeSNat toSomeSNat
