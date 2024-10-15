module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.TreeSitter.CApi qualified as CApi

main :: IO ()
main = do
  defaultMain $
    testGroup "TreeSitter" [CApi.tests]
