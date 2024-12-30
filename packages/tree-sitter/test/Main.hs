{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor ((<&>))
import Test.Tasty (defaultMain, testGroup, localOption)
import Test.TreeSitter.Corpus (makeCorpusTests)
import Test.TreeSitter.Internal qualified as Internal
import TreeSitter.JavaScript qualified as JavaScript
import TreeSitter.While qualified as While
import Test.Tasty.Runners (TestPattern (..))
import Test.Tasty.Patterns.Types (Expr(..))

--------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Avoid partial function" :: String) #-}

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Generate JavaScript corpus tests
  javascriptCorpusTests <-
    JavaScript.getTestCorpusDir
      >>= makeCorpusTests JavaScript.tree_sitter_javascript
      <&> testGroup "Corpus"
  -- TODO: These failing tests are the result of the corpus parser,
  --       which is not being respectful and should be parsing the
  --       contents of the corpus files as a ByteString.
  let ignoreKnownFailures = TestPattern . Just . foldr1 And $
        [ Not (ERE "Non-breaking spaces as whitespace")
        , Not (ERE "U+2028 as a line terminator")
        , Not (ERE "Unicode identifiers")
        ]
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
      , localOption ignoreKnownFailures javascriptCorpusTests
      , whileCorpusTests
      ]
