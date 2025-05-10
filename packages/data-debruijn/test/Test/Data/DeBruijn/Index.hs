{-# LANGUAGE RecordWildCards #-}

module Test.Data.DeBruijn.Index (tests) where

import Data.DeBruijn.Index (Ix, SomeIx (..))
import Data.DeBruijn.Index qualified as Ix
import Data.DeBruijn.Index.Arbitrary qualified as Ix (arbitraryIx)
import Data.DeBruijn.Index.Inductive qualified as Ix (toInductive)
import Data.DeBruijn.Index.Inductive qualified as Ix.Inductive
import Data.Proxy (Proxy (..))
import Data.Type.Nat (Nat (..))
import Data.Type.Nat.Singleton (SNat (..), SomeSNat (..))
import Data.Type.Nat.Singleton.Inductive qualified as SNat (toInductive)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen, testProperty)

tests :: TestTree
tests =
  testGroup
    "Test.DeBruijn.Data.Index"
    [ testProperty "test_eqIxEq" test_eqIxEq
    , testProperty "test_fromIxRawEq" test_fromIxRawEq
    , testProperty "test_fromIxEq" test_fromIxEq
    , testProperty "test_injectEq" test_injectEq
    , testProperty "test_thinEq" test_thinEq
    , testProperty "test_thickEq" test_thickEq
    ]

test_eqIxEq :: SomeIx -> SomeIx -> Bool
test_eqIxEq (SomeIx{index= i, bound = _}) (SomeIx{index = j, bound = _}) =
  Ix.Inductive.eqIx (Ix.toInductive i) (Ix.toInductive j) == Ix.eqIx i j

test_fromIxRawEq :: SomeIx -> Bool
test_fromIxRawEq (SomeIx{..}) =
   Ix.Inductive.fromIxRaw (Ix.toInductive index) == Ix.fromIxRaw index

test_fromIxEq :: SomeIx -> Bool
test_fromIxEq (SomeIx{..}) =
   Ix.Inductive.fromIx @Int (Ix.toInductive index) == Ix.fromIx @Int index

test_injectEq :: SomeSNat -> SomeIx -> Bool
test_injectEq (SomeSNat n) (SomeIx{..}) =
  Ix.Inductive.inject (SNat.toInductive n) (Ix.toInductive index) == Ix.Inductive.toInductive (Ix.inject (erase n) index)

prop_thinEq :: Ix (S n) -> Ix n -> Bool
prop_thinEq i j = Ix.Inductive.thin (Ix.toInductive i) (Ix.toInductive j) == Ix.toInductive (Ix.thin i j)

test_thinEq :: Gen Bool
test_thinEq = do
  SomeSNat n <- arbitrary
  i <- Ix.arbitraryIx (S (S n))
  j <- Ix.arbitraryIx (S n)
  pure $ prop_thinEq i j

prop_thickEq :: Ix (S n) -> Ix (S n) -> Bool
prop_thickEq i j = Ix.Inductive.thick (Ix.toInductive i) (Ix.toInductive j) == (Ix.toInductive <$> Ix.thick i j)

test_thickEq :: Gen Bool
test_thickEq = do
  SomeSNat n <- arbitrary
  i <- Ix.arbitraryIx (S n)
  j <- Ix.arbitraryIx (S n)
  pure $ prop_thickEq i j

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | @`erase` x@ erases the content of @x@ to a @`Proxy`@.
erase :: f a -> Proxy a
erase _ = Proxy
{-# INLINE erase #-}
