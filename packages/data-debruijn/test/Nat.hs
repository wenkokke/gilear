module Nat (tests) where

import Data.Type.Nat.Singleton
import Data.Type.Nat.Singleton.Arbitrary ()
import qualified Data.Type.Nat.Singleton.Inductive as Ind
import Data.Type.Nat.Singleton.Inductive.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Nat tests"
    [ testProperty "fromSNatRawEq" fromSNatRawEq,
      testProperty "fromSNatEq" fromSNatEq,
      testProperty "decSNatEq" decSNatEq
    ]

fromSNatRawEq :: SomeSNat -> Bool
fromSNatRawEq (SomeSNat n) = fromSNatRaw n == Ind.fromSNatRaw (Ind.toInductive n)

fromSNatEq :: SomeSNat -> Bool
fromSNatEq (SomeSNat n) = fromSNat n == Ind.fromSNat (Ind.toInductive n)

decSNatEq :: SomeSNat -> SomeSNat -> Bool
decSNatEq (SomeSNat m) (SomeSNat n) = decSNat m n == Ind.decSNat (Ind.toInductive m) (Ind.toInductive n)
