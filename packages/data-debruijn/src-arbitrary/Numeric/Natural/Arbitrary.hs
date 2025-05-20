{-# OPTIONS_GHC -Wno-orphans #-}

module Numeric.Natural.Arbitrary () where

import Numeric.Natural (Natural)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..), arbitrarySizedNatural, coarbitraryIntegral, shrinkIntegral)
import Test.QuickCheck.Function (Function (..), functionIntegral, (:->))
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
