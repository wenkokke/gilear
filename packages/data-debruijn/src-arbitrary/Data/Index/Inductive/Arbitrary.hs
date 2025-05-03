{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Index.Inductive.Arbitrary where

import Data.Index.Inductive ( SomeIx, Ix, toSomeIx, fromSomeIx )
import Data.Type.Nat ( Nat(Z, S) )
import Test.QuickCheck.Arbitrary
    ( shrinkIntegral, Arbitrary(..), CoArbitrary(..) )
import Test.QuickCheck.Function
    ( functionMap, (:->), Function(..) )
import Test.QuickCheck.Gen ( Gen )

instance Arbitrary SomeIx where
  arbitrary :: Gen SomeIx
  arbitrary = fmap toSomeIxRaw arbitrary

  shrink :: SomeIx -> [SomeIx]
  shrink = fmap toSomeIxRaw . shrinkIntegral . fromSomeIx

instance CoArbitrary SomeIx where
  coarbitrary :: SomeIx -> Gen b -> Gen b
  coarbitrary = coarbitrary . fromSomeIxRaw

instance Function SomeIx where
  function :: (SomeIx -> b) -> SomeIx :-> b
  function = functionMap fromSomeIxRaw toSomeIx

instance Arbitrary (Ix (S Z)) where
  arbitrary :: Gen (Ix (S Z))
  arbitrary = undefined

  shrink :: Ix (S Z) -> [Ix (S Z)]
  shrink = undefined

instance Arbitrary (Ix (S (S n))) where
  arbitrary :: Gen (Ix (S (S n)))
  arbitrary = undefined

  shrink :: Ix (S (S n)) -> [Ix (S (S n))]
  shrink = undefined



------------
-- instance CoArbitrary n => CoArbitrary (Ix n) where
--   coarbitrary :: Ix n -> Gen b -> Gen b
--   coarbitrary = coarbitrary . undefined

-- instance Function n => Function (Ix n) where
--   function :: (Ix n -> b) -> Ix n :-> b
--   function = functionMap undefined undefined
