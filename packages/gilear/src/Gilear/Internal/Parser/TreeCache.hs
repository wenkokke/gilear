{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.Internal.Parser.TreeCache where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Kind (Type)
import Language.LSP.Protocol.Types (NormalizedUri)
import TreeSitter qualified as TS

-- | Collection of ASTs for all open files.
type TreeCache :: Type
newtype TreeCache = TreeCache {unTreeCache :: HashMap NormalizedUri TS.Tree}

empty :: TreeCache
empty = TreeCache M.empty

insert :: NormalizedUri -> TS.Tree -> TreeCache -> TreeCache
insert uri tree treeCache =
  TreeCache $ M.insert uri tree (unTreeCache treeCache)

lookup :: NormalizedUri -> TreeCache -> Maybe TS.Tree
lookup uri treeCache =
  M.lookup uri (unTreeCache treeCache)
