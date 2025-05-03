{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Type.Nat.Singleton.Inductive.Arbitrary () where

import Data.Type.Nat.Singleton.Inductive (SomeSNat (..), fromSomeSNat, toSomeSNat)
import Numeric.Natural (Natural)
import Numeric.Natural.Arbitrary ()
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..), shrinkIntegral)
import Test.QuickCheck.Function (Function (..), functionMap, (:->))
import Test.QuickCheck.Gen (Gen)

--------------------------------------------------------------------------------
-- QuickCheck instances for SomeSNat
--------------------------------------------------------------------------------

instance Arbitrary SomeSNat where
  arbitrary :: Gen SomeSNat
  arbitrary = fmap (toSomeSNat @Natural) arbitrary

  shrink :: SomeSNat -> [SomeSNat]
  shrink = fmap (toSomeSNat @Natural) . shrinkIntegral . (fromSomeSNat @Natural)

instance CoArbitrary SomeSNat where
  coarbitrary :: SomeSNat -> Gen b -> Gen b
  coarbitrary = coarbitrary . (fromSomeSNat @Natural)

instance Function SomeSNat where
  function :: (SomeSNat -> b) -> SomeSNat :-> b
  function = functionMap (fromSomeSNat @Natural) (toSomeSNat @Natural)
