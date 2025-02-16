{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Gilear.Internal.Parser.Cache (
  ParserCache,
  ParserCacheItem (..),
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
import Gilear.Internal.Parser.Ast (Ast, AstCache)
import TreeSitter (Tree)
import Prelude hiding (lookup)

type ParserCacheItem :: Type
data ParserCacheItem = ParserCacheItem
  { itemRope :: !Rope
  , itemTree :: !Tree
  , itemDiag :: !Diagnostics
  , itemAst :: !Ast
  , itemAstCache :: !AstCache
  }

-- | Collection of ASTs for all open files.
type ParserCache :: Type -> Type
newtype ParserCache uri = ParserCache {unParserCache :: HashMap uri ParserCacheItem}

empty :: ParserCache uri
empty = ParserCache M.empty

insert :: (Hashable uri) => uri -> ParserCacheItem -> ParserCache uri -> ParserCache uri
insert uri tree cache = ParserCache $ M.insert uri tree (unParserCache cache)

delete :: (Hashable uri) => uri -> ParserCache uri -> ParserCache uri
delete uri cache = ParserCache $ M.delete uri (unParserCache cache)

lookup :: (Hashable uri) => uri -> ParserCache uri -> Maybe ParserCacheItem
lookup uri cache = M.lookup uri (unParserCache cache)

adjust :: (Hashable uri) => (ParserCacheItem -> ParserCacheItem) -> uri -> ParserCache uri -> ParserCache uri
adjust f uri cache = ParserCache $ M.adjust f uri (unParserCache cache)
