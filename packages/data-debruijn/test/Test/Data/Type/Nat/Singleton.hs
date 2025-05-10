module Test.Data.Type.Nat.Singleton (tests) where

import Data.Type.Nat.Singleton (SomeSNat (..))
import Data.Type.Nat.Singleton qualified as SNat
import Data.Type.Nat.Singleton.Arbitrary ()
import Data.Type.Nat.Singleton.Inductive qualified as SNat (toInductive)
import Data.Type.Nat.Singleton.Inductive qualified as SNat.Inductive
import Data.Type.Nat.Singleton.Inductive.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (NonNegative (..), testProperty)

tests :: TestTree
tests =
  testGroup
    "Test.Data.Type.Nat.Singleton"
    [ -- Test correspondence between Efficient and Inductive APIs
      testProperty "test_fromSNatRawEq" test_fromSNatRawEq
    , testProperty "test_fromSNatEq" test_fromSNatEq
    , testProperty "test_decSNatEq" test_decSNatEq
    , -- Test conversion to/from numbers of Efficient API
      testProperty "test_Efficient_fromSomeSNat_eq_fromSomeSNatRaw" test_Efficient_fromSomeSNat_eq_fromSomeSNatRaw
    , testProperty "test_Efficient_toSomeSNat_eq_toSomeSNatRaw" test_Efficient_toSomeSNat_eq_toSomeSNatRaw
    , testProperty "test_Efficient_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id" test_Efficient_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id
    , testProperty "test_Efficient_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id" test_Efficient_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id
    , -- Test conversion to/from numbers of Inductive API
      testProperty "test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw" test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw
    , testProperty "test_Inductive_toSomeSNat_eq_toSomeSNatRaw" test_Inductive_toSomeSNat_eq_toSomeSNatRaw
    , testProperty "test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id" test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id
    , testProperty "test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id" test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id
    ]

--------------------------------------------------------------------------------
-- Test correspondence between Efficient and Inductive APIs
--------------------------------------------------------------------------------

test_fromSNatRawEq :: SomeSNat -> Bool
test_fromSNatRawEq (SomeSNat n) =
  SNat.Inductive.fromSNatRaw (SNat.toInductive n) == SNat.fromSNatRaw n

test_fromSNatEq :: SomeSNat -> Bool
test_fromSNatEq (SomeSNat n) =
  SNat.Inductive.fromSNat @Int (SNat.toInductive n) == SNat.fromSNat @Int n

test_decSNatEq :: SomeSNat -> SomeSNat -> Bool
test_decSNatEq (SomeSNat m) (SomeSNat n) =
  SNat.decSNat m n == SNat.Inductive.decSNat (SNat.toInductive m) (SNat.toInductive n)

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Efficient API
--------------------------------------------------------------------------------

test_Efficient_fromSomeSNat_eq_fromSomeSNatRaw :: SomeSNat -> Bool
test_Efficient_fromSomeSNat_eq_fromSomeSNatRaw n =
  SNat.fromSomeSNat n == SNat.fromSomeSNatRaw n

test_Efficient_toSomeSNat_eq_toSomeSNatRaw :: NonNegative Int -> Bool
test_Efficient_toSomeSNat_eq_toSomeSNatRaw (NonNegative u) =
  SNat.toSomeSNat u == SNat.toSomeSNatRaw u

test_Efficient_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: SomeSNat -> Bool
test_Efficient_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n =
  SNat.toSomeSNatRaw (SNat.fromSomeSNatRaw n) == n

test_Efficient_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative Int -> Bool
test_Efficient_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative u) =
  SNat.fromSomeSNatRaw (SNat.toSomeSNatRaw u) == u

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Inductive API
--------------------------------------------------------------------------------

test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw :: SNat.Inductive.SomeSNat -> Bool
test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw n =
  SNat.Inductive.fromSomeSNat n == SNat.Inductive.fromSomeSNatRaw n

test_Inductive_toSomeSNat_eq_toSomeSNatRaw :: NonNegative Int -> Bool
test_Inductive_toSomeSNat_eq_toSomeSNatRaw (NonNegative u) =
  SNat.Inductive.toSomeSNat u == SNat.Inductive.toSomeSNatRaw u

test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: SNat.Inductive.SomeSNat -> Bool
test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n =
  SNat.Inductive.toSomeSNatRaw (SNat.Inductive.fromSomeSNatRaw n) == n

test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative Int -> Bool
test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative u) =
  SNat.Inductive.fromSomeSNatRaw (SNat.Inductive.toSomeSNatRaw u) == u
