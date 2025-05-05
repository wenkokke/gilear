module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Nat qualified
import Index qualified
import Thinning qualified

tests :: TestTree
tests = testGroup "All Tests" [Nat.tests, Index.tests, Thinning.tests]

main :: IO ()
main = defaultMain tests
