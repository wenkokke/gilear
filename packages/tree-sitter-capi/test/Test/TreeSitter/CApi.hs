module Test.TreeSitter.CApi where

import Control.Exception (bracket)
import Foreign (castPtr, nullPtr)
import Foreign.C (withCStringLen)
import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import TreeSitter.CApi (
  ts_language_delete,
  ts_parser_delete,
  ts_parser_new,
  ts_parser_parse_string,
 )
import TreeSitter.While (tree_sitter_while)

tests :: TestTree
tests =
  testGroup
    "CApi"
    [ test_ts_language_delete
    , test_ts_parser_parse_fails_without_language
    ]

-- | Is deleting a @`TreeSitter.CApi.TSLanguage`@ multiple times safe?
test_ts_language_delete :: TestTree
test_ts_language_delete =
  testCase "ts_language_delete" $ do
    ts_language <- castConstPtr <$> tree_sitter_while
    ts_language_delete ts_language
    ts_language_delete ts_language

-- | Does @`ts_parser_parse_string`@ return @NULL@ when the parser does not have a language assigned.
test_ts_parser_parse_fails_without_language :: TestTree
test_ts_parser_parse_fails_without_language =
  testCase "test_ts_parser_parse fails without language" $ do
    bracket ts_parser_new ts_parser_delete $ \ts_parser ->
      withCStringLen "" $ \(string, string_length) -> do
        tree_p <-
          ts_parser_parse_string
            ts_parser
            (ConstPtr nullPtr)
            (ConstPtr string)
            (fromIntegral string_length)
        nullPtr @=? tree_p

castConstPtr :: ConstPtr a -> ConstPtr b
castConstPtr = ConstPtr . castPtr . unConstPtr
