module Main (main) where

import Test.Data.Index qualified (tests)
import Test.Data.Thinning qualified (tests)
import Test.Data.Type.Nat.Singleton qualified (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ Test.Data.Index.tests
    , Test.Data.Thinning.tests
    , Test.Data.Type.Nat.Singleton.tests
    ]

main :: IO ()
main = defaultMain tests
