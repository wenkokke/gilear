module TreeSitter.Gilear (
  tree_sitter_gilear,
  getNodeTypesPath,
  getTestCorpusDir,
) where

import Foreign.C.ConstPtr.Compat (ConstPtr (..))
import Paths_tree_sitter_gilear (getDataFileName)
import TreeSitter.CApi (TSLanguage)

foreign import ccall unsafe "src/parser.c tree_sitter_gilear"
  tree_sitter_gilear :: IO (ConstPtr TSLanguage)

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "test/corpus"
