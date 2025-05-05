{-# LANGUAGE RecordWildCards #-}

module Index (tests) where

import Data.Index
import Data.Index.Arbitrary ()
import qualified Data.Index.Inductive as Ind
import Data.Index.Inductive.Arbitrary ()
import Data.Type.Nat.Singleton
import qualified Data.Type.Nat.Singleton.Inductive as Nat
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Index tests"
    [ testProperty "eqIxEq" eqIxEq,
      testProperty "fromIxRawEq" fromIxRawEq,
      testProperty "fromIxEq" fromIxEq --,
      -- testProperty "injectEq" injectEq,
      -- testProperty "thickEq" thickEq,
      -- testProperty "thinEq" thinEq
    ]

eqIxEq :: SomeIx -> SomeIx -> Bool
eqIxEq (SomeIx {..}) (SomeIx {bound = _, index = index'}) =
  eqIx index index' == Ind.eqIx (Ind.toInductive index) (Ind.toInductive index')

fromIxRawEq :: SomeIx -> Bool
fromIxRawEq (SomeIx {..}) =
  fromIxRaw index == Ind.fromIxRaw (Ind.toInductive index)

fromIxEq :: SomeIx -> Bool
fromIxEq (SomeIx {..}) =
  fromIx index == Ind.fromIx (Ind.toInductive index)

-- injectEq :: SomeSNat -> SomeIx -> Bool
-- injectEq (SomeSNat n) (SomeIx {..}) =
--   inject n index == Ind.inject (Nat.toInductive n) (Ind.toInductive index)

-- thickEq :: SomeIx -> SomeIx -> Bool
-- thickEq (SomeIx {..}) (SomeIx {bound = _, index = index'}) =
--   thick index index' == (Ind.fromInductive <$> Ind.thick (Ind.toInductive index) (Ind.toInductive index'))

-- thinEq :: SomeIx -> SomeIx -> Bool
-- thinEq (SomeIx {..}) (SomeIx {bound = _, index = index'}) =
--   thin index index' == Ind.thin (Ind.toInductive index) (Ind.toInductive index')
