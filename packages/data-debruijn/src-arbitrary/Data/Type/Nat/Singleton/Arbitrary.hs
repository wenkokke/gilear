{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Type.Nat.Singleton.Arbitrary where

import Data.Type.Nat.Singleton (SomeSNat (..), fromSomeSNat, toSomeSNat)
import Data.Word (Word16)
import Numeric.Natural.Arbitrary ()
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..), shrinkIntegral)
import Test.QuickCheck.Function (Function (..), functionMap, (:->))
import Test.QuickCheck.Gen (Gen)

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
