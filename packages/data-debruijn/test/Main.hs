module Main (main) where

import Test.Data.DeBruijn.Index qualified (tests)
import Test.Data.DeBruijn.Thinning qualified (tests)
import Test.Data.Type.Nat.Singleton qualified (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ Test.Data.DeBruijn.Index.tests
    , Test.Data.DeBruijn.Thinning.tests
    , Test.Data.Type.Nat.Singleton.tests
    ]

main :: IO ()
main = defaultMain tests
