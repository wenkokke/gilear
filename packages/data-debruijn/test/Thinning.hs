module Thinning (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Thinning tests"
    [ testProperty "" $ undefined
    ]
