{-# LANGUAGE RecordWildCards #-}

module Test.Data.Index (tests) where

import Data.Index (SomeIx (..))
import Data.Index qualified as Ix
import Data.Index.Arbitrary ()
import Data.Index.Inductive qualified as Ix (toInductive)
import Data.Index.Inductive qualified as Ix.Inductive
import Data.Index.Inductive.Arbitrary ()
import Data.Type.Nat.Singleton ()
-- import Data.Type.Nat.Singleton.Inductive qualified as SNat (toInductive)
-- import Data.Type.Nat.Singleton.Inductive qualified as SNat.Inductive
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Index tests"
    [ testProperty "eqIxEq" eqIxEq
    , testProperty "fromIxRawEq" fromIxRawEq
    , testProperty "fromIxEq" fromIxEq -- ,
    -- testProperty "injectEq" injectEq,
    -- testProperty "thickEq" thickEq,
    -- testProperty "thinEq" thinEq
    ]

eqIxEq :: SomeIx -> SomeIx -> Bool
eqIxEq (SomeIx{..}) (SomeIx{bound = _, index = index'}) =
  Ix.eqIx index index' == Ix.Inductive.eqIx (Ix.toInductive index) (Ix.toInductive index')

fromIxRawEq :: SomeIx -> Bool
fromIxRawEq (SomeIx{..}) =
  Ix.fromIxRaw index == Ix.Inductive.fromIxRaw (Ix.toInductive index)

fromIxEq :: SomeIx -> Bool
fromIxEq (SomeIx{..}) =
  Ix.fromIx @Int index == Ix.Inductive.fromIx @Int (Ix.toInductive index)

-- injectEq :: SomeSNat -> SomeIx -> Bool
-- injectEq (SomeSNat n) (SomeIx {..}) =
--   inject n index == Inductive.inject (Nat.toInductive n) (toInductive index)

-- thickEq :: SomeIx -> SomeIx -> Bool
-- thickEq (SomeIx {..}) (SomeIx {bound = _, index = index'}) =
--   thick index index' == (Inductive.fromInductive <$> Inductive.thick (toInductive index) (toInductive index'))

-- thinEq :: SomeIx -> SomeIx -> Bool
-- thinEq (SomeIx {..}) (SomeIx {bound = _, index = index'}) =
--   thin index index' == Inductive.thin (toInductive index) (toInductive index')
