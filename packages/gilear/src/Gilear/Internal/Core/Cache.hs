{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.Internal.Core.Cache (
  Cache,
  CacheItem (..),
  empty,
  insert,
  delete,
  lookup,
  adjust,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Text.Mixed.Rope (Rope)
import Gilear.Internal.Core.Diagnostics (Diagnostics)
import TreeSitter (Tree)
import Prelude hiding (lookup)

type CacheItem :: Type
data CacheItem = CacheItem
  { itemRope :: !Rope
  , itemTree :: !Tree
  , itemDiagnostics :: !Diagnostics
  }

-- | Collection of ASTs for all open files.
type Cache :: Type -> Type
newtype Cache uri = Cache {unCache :: HashMap uri CacheItem}

empty :: Cache uri
empty = Cache M.empty

insert :: (Hashable uri) => uri -> CacheItem -> Cache uri -> Cache uri
insert uri tree cache = Cache $ M.insert uri tree (unCache cache)

delete :: (Hashable uri) => uri -> Cache uri -> Cache uri
delete uri cache = Cache $ M.delete uri (unCache cache)

lookup :: (Hashable uri) => uri -> Cache uri -> Maybe CacheItem
lookup uri cache = M.lookup uri (unCache cache)

adjust :: (Hashable uri) => (CacheItem -> CacheItem) -> uri -> Cache uri -> Cache uri
adjust f uri cache = Cache $ M.adjust f uri (unCache cache)
