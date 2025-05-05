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
    "Nat tests"
    [ testProperty "fromSNatRawEq" fromSNatRawEq
    , testProperty "fromSNatEq" fromSNatEq
    , testProperty "decSNatEq" decSNatEq
    ]

fromSNatRawEq :: SomeSNat -> Bool
fromSNatRawEq (SomeSNat n) =
  SNat.fromSNatRaw n == SNat.Inductive.fromSNatRaw (SNat.toInductive n)

fromSNatEq :: SomeSNat -> Bool
fromSNatEq (SomeSNat n) =
  SNat.fromSNat @Int n == SNat.Inductive.fromSNat @Int (SNat.toInductive n)

decSNatEq :: SomeSNat -> SomeSNat -> Bool
decSNatEq (SomeSNat m) (SomeSNat n) =
  SNat.decSNat m n == SNat.Inductive.decSNat (SNat.toInductive m) (SNat.toInductive n)
