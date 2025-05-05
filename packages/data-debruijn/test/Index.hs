module Index (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Index tests"
    [ testProperty "" $ undefined
    ]
