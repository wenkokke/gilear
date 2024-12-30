{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor ((<&>))
import Test.Tasty (defaultMain, testGroup)
import Test.TreeSitter.Corpus (makeCorpusTests)
import Test.TreeSitter.Internal qualified as Internal
import TreeSitter.While (getTestCorpusDir, tree_sitter_while)

main :: IO ()
main = do
  whileCorpusTests <-
    getTestCorpusDir
      >>= makeCorpusTests tree_sitter_while
      <&> testGroup "Corpus"
  defaultMain $
    testGroup
      "TreeSitter"
      [ Internal.tests
      , whileCorpusTests
      ]

-- TODO: Add SExp parser to properly test equality.
