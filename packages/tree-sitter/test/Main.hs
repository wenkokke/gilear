{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor ((<&>))
import Test.Tasty (defaultMain, testGroup)
import Test.TreeSitter.Corpus (makeCorpusTests)
import Test.TreeSitter.Internal qualified as Internal
import TreeSitter.JavaScript qualified as JavaScript
import TreeSitter.While qualified as While

main :: IO ()
main = do
  -- Generate JavaScript corpus tests
  javascriptCorpusTests <-
    JavaScript.getTestCorpusDir
      >>= makeCorpusTests JavaScript.tree_sitter_javascript
      <&> testGroup "Corpus"
  -- Generate WHILE corpus tests
  whileCorpusTests <-
    While.getTestCorpusDir
      >>= makeCorpusTests While.tree_sitter_while
      <&> testGroup "Corpus"
  -- Compose test suite
  defaultMain $
    testGroup
      "TreeSitter"
      [ Internal.tests
      , javascriptCorpusTests
      , whileCorpusTests
      ]
