module Gilear.Internal.Parser.Core (
  Parser,
  new,
) where

import Control.Exception (assert)
import TreeSitter (Parser)
import TreeSitter qualified as TS
import TreeSitter.Gilear qualified as TS (tree_sitter_gilear)

new :: IO Parser
new = do
  parser <- TS.parserNew
  language <- TS.unsafeToLanguage =<< TS.tree_sitter_gilear
  success <- TS.parserSetLanguage parser language
  -- TODO: Report an error rather than dying.
  assert success $ pure parser
