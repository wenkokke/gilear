module Main (main) where

import Test.Tasty

tests :: TestTree
tests = testGroup "All Tests" []

main :: IO ()
main = defaultMain Main.tests
