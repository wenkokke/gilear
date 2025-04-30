module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests = testGroup "All Tests" []

main :: IO ()
main = defaultMain Main.tests
