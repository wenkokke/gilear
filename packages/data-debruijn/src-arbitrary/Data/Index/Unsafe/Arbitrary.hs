{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Index.Unsafe.Arbitrary where

import qualified Data.Index.Inductive as Inductive
import Data.Index.Inductive.Arbitrary ()
import Data.Index.Unsafe
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Function
import Test.QuickCheck.Gen

-- instance Arbitrary n => Arbitrary (Ix n) where
--   arbitrary :: Gen (Ix n)
--   arbitrary = fmap (toEfficient @Inductive.Ix) arbitrary

--   shrink :: Ix n -> [Ix n]
--   shrink = fmap (toEfficient @Inductive.Ix) shrink

-- instance CoArbitrary n => CoArbitrary (Ix n) where
--   coarbitrary :: Ix n -> Gen b -> Gen b
--   coarbitrary = coarbitrary . (fromEfficient @Inductive.Ix)

-- instance Function n => Function (Ix n) where
--   function :: (Ix n -> b) -> Ix n :-> b
--   function = functionMap (fromEfficient @Inductive.Ix) toEfficient
