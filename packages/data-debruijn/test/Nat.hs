module Nat (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Nat tests"
    [ testProperty "" $ undefined
    ]
