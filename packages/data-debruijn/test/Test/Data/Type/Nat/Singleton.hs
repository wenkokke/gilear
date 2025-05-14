module Test.Data.Type.Nat.Singleton (tests) where

import Data.Type.Nat.Singleton qualified as Unsafe
import Data.Type.Nat.Singleton.Arbitrary ()
import Data.Type.Nat.Singleton.Inductive qualified as Inductive
import Data.Type.Nat.Singleton.Inductive qualified as Unsafe (fromInductive)
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

test_fromSNatRawEq :: Inductive.SomeSNat -> Bool
test_fromSNatRawEq (Inductive.SomeSNat n) =
  Inductive.fromSNatRaw n == Unsafe.fromSNatRaw (Unsafe.fromInductive n)

test_fromSNatEq :: Inductive.SomeSNat -> Bool
test_fromSNatEq (Inductive.SomeSNat n) =
  Inductive.fromSNat @Int n == Unsafe.fromSNat @Int (Unsafe.fromInductive n)

test_decSNatEq :: Inductive.SomeSNat -> Inductive.SomeSNat -> Bool
test_decSNatEq (Inductive.SomeSNat m) (Inductive.SomeSNat n) =
  Inductive.decSNat m n == Unsafe.decSNat (Unsafe.fromInductive m) (Unsafe.fromInductive n)

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Efficient API
--------------------------------------------------------------------------------

test_Efficient_fromSomeSNat_eq_fromSomeSNatRaw :: Unsafe.SomeSNat -> Bool
test_Efficient_fromSomeSNat_eq_fromSomeSNatRaw n =
  Unsafe.fromSomeSNat n == Unsafe.fromSomeSNatRaw n

test_Efficient_toSomeSNat_eq_toSomeSNatRaw :: NonNegative Int -> Bool
test_Efficient_toSomeSNat_eq_toSomeSNatRaw (NonNegative u) =
  Unsafe.toSomeSNat u == Unsafe.toSomeSNatRaw u

test_Efficient_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: Unsafe.SomeSNat -> Bool
test_Efficient_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n =
  Unsafe.toSomeSNatRaw (Unsafe.fromSomeSNatRaw n) == n

test_Efficient_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative Int -> Bool
test_Efficient_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative u) =
  Unsafe.fromSomeSNatRaw (Unsafe.toSomeSNatRaw u) == u

--------------------------------------------------------------------------------
-- Test conversion to/from numbers of Inductive API
--------------------------------------------------------------------------------

test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw :: Inductive.SomeSNat -> Bool
test_Inductive_fromSomeSNat_eq_fromSomeSNatRaw n =
  Inductive.fromSomeSNat n == Inductive.fromSomeSNatRaw n

test_Inductive_toSomeSNat_eq_toSomeSNatRaw :: NonNegative Int -> Bool
test_Inductive_toSomeSNat_eq_toSomeSNatRaw (NonNegative u) =
  Inductive.toSomeSNat u == Inductive.toSomeSNatRaw u

test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id :: Inductive.SomeSNat -> Bool
test_Inductive_toSomeSNatRaw_o_fromSomeSNatRaw_eq_id n =
  Inductive.toSomeSNatRaw (Inductive.fromSomeSNatRaw n) == n

test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id :: NonNegative Int -> Bool
test_Inductive_fromSomeSNatRaw_o_toSomeSNatRaw_eq_id (NonNegative u) =
  Inductive.fromSomeSNatRaw (Inductive.toSomeSNatRaw u) == u
