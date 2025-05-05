module Test.Data.Type.Nat.Singleton (tests) where

import Data.Type.Nat.Singleton (SomeSNat (..))
import Data.Type.Nat.Singleton qualified as SNat
import Data.Type.Nat.Singleton.Arbitrary ()
import Data.Type.Nat.Singleton.Inductive qualified as SNat (toInductive)
import Data.Type.Nat.Singleton.Inductive qualified as SNat.Inductive
import Data.Type.Nat.Singleton.Inductive.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Test.Data.Type.Nat.Singleton"
    [ testProperty "test_fromSNatRawEq" test_fromSNatRawEq
    , testProperty "test_fromSNatEq" test_fromSNatEq
    , testProperty "test_decSNatEq" test_decSNatEq
    ]

test_fromSNatRawEq :: SomeSNat -> Bool
test_fromSNatRawEq (SomeSNat n) =
  SNat.fromSNatRaw n == SNat.Inductive.fromSNatRaw (SNat.toInductive n)

test_fromSNatEq :: SomeSNat -> Bool
test_fromSNatEq (SomeSNat n) =
  SNat.fromSNat @Int n == SNat.Inductive.fromSNat @Int (SNat.toInductive n)

test_decSNatEq :: SomeSNat -> SomeSNat -> Bool
test_decSNatEq (SomeSNat m) (SomeSNat n) =
  SNat.decSNat m n == SNat.Inductive.decSNat (SNat.toInductive m) (SNat.toInductive n)
