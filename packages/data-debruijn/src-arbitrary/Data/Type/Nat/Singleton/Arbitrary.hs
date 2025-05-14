{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Type.Nat.Singleton.Arbitrary () where

import Data.Type.Nat.Singleton (SomeSNat (..), fromSomeSNatRaw, toSomeSNatRaw)
import Numeric.Natural.Arbitrary ()
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..), shrinkIntegral)
import Test.QuickCheck.Function (Function (..), functionMap, (:->))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Modifiers (NonNegative (..))

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeSNat
--------------------------------------------------------------------------------

instance Arbitrary SomeSNat where
  arbitrary :: Gen SomeSNat
  arbitrary = fmap (toSomeSNatRaw . getNonNegative) arbitrary

  shrink :: SomeSNat -> [SomeSNat]
  shrink = fmap toSomeSNatRaw . shrinkIntegral . fromSomeSNatRaw

instance CoArbitrary SomeSNat where
  coarbitrary :: SomeSNat -> Gen b -> Gen b
  coarbitrary = coarbitrary . fromSomeSNatRaw

instance Function SomeSNat where
  function :: (SomeSNat -> b) -> SomeSNat :-> b
  function = functionMap fromSomeSNatRaw toSomeSNatRaw
