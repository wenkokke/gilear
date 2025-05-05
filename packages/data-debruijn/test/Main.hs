module Main (main) where

import Index qualified
import Nat qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Thinning qualified

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ Nat.tests,
      Index.tests --,
      -- Thinning.tests
    ]

main :: IO ()
main = defaultMain tests
