{-# LANGUAGE RecordWildCards #-}

module Test.Data.DeBruijn.Index (tests) where

import Data.DeBruijn.Index (SomeIx (..))
import Data.DeBruijn.Index qualified as Ix
import Data.DeBruijn.Index.Arbitrary ()
import Data.DeBruijn.Index.Inductive qualified as Ix (toInductive)
import Data.DeBruijn.Index.Inductive qualified as Ix.Inductive
import Data.DeBruijn.Index.Inductive.Arbitrary ()
import Data.Type.Nat.Singleton ()
-- import Data.Type.Nat.Singleton.Inductive qualified as SNat (toInductive)
-- import Data.Type.Nat.Singleton.Inductive qualified as SNat.Inductive
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Test.DeBruijn.Data.Index"
    [ testProperty "test_eqIxEq" test_eqIxEq
    , testProperty "test_fromIxRawEq" test_fromIxRawEq
    , testProperty "test_fromIxEq" test_fromIxEq -- ,
    -- testProperty "test_injectEq" test_injectEq,
    -- testProperty "test_thickEq" test_thickEq,
    -- testProperty "test_thinEq" test_thinEq
    ]

test_eqIxEq :: SomeIx -> SomeIx -> Bool
test_eqIxEq (SomeIx{..}) (SomeIx{bound = _, index = index'}) =
  Ix.eqIx index index' == Ix.Inductive.eqIx (Ix.toInductive index) (Ix.toInductive index')

test_fromIxRawEq :: SomeIx -> Bool
test_fromIxRawEq (SomeIx{..}) =
  Ix.fromIxRaw index == Ix.Inductive.fromIxRaw (Ix.toInductive index)

test_fromIxEq :: SomeIx -> Bool
test_fromIxEq (SomeIx{..}) =
  Ix.fromIx @Int index == Ix.Inductive.fromIx @Int (Ix.toInductive index)

-- test_injectEq :: SomeSNat -> SomeIx -> Bool
-- test_injectEq (SomeSNat n) (SomeIx {..}) =
--   inject n index == Inductive.inject (Nat.toInductive n) (toInductive index)

-- test_thickEq :: SomeIx -> SomeIx -> Bool
-- test_thickEq (SomeIx {..}) (SomeIx {bound = _, index = index'}) =
--   thick index index' == (Inductive.fromInductive <$> Inductive.thick (toInductive index) (toInductive index'))

-- test_thinEq :: SomeIx -> SomeIx -> Bool
-- test_thinEq (SomeIx {..}) (SomeIx {bound = _, index = index'}) =
--   thin index index' == Inductive.thin (toInductive index) (toInductive index')
