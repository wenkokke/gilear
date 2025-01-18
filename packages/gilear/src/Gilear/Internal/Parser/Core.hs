{-# LANGUAGE RecordWildCards #-}

module Gilear.Internal.Parser.Core (
  Parser,
  newParser,
  ParserEnv (..),
  newParserEnv,
) where

import Control.Monad (unless)
import Data.IORef (IORef, newIORef)
import Data.Kind (Type)
import Gilear.Internal.Parser.Cache (ParserCache)
import Gilear.Internal.Parser.Cache qualified as Cache
import TreeSitter (Parser)
import TreeSitter qualified as TS
import TreeSitter.Gilear (tree_sitter_gilear)

--------------------------------------------------------------------------------
-- Parser
--
-- NOTE: These functions are not defined in `Gilear.Internal.Parser` in order to
--       avoid a cyclic dependency.
--------------------------------------------------------------------------------

-- | Create a new tree-sitter parser environment.
newParser :: IO Parser
newParser = do
  -- Initialise a new tree-sitter parser
  parser <- TS.parserNew
  -- Initialise the tree-sitter-gilear language
  language <- TS.unsafeToLanguage =<< tree_sitter_gilear
  -- Configure the parser
  success <- TS.parserSetLanguage parser language
  unless success $
    -- TODO: report an error rather than dying
    fail "gilear: failed to set parser language"
  -- Return the parser environment
  pure parser

--------------------------------------------------------------------------------
-- Parser Environments
--------------------------------------------------------------------------------

{-| Parser Environment.

  This is an environment, rather than state, because it is intended to be
  used with the reader monad, rather than the state monad. However, it is
  intended to hold references to mutable state.
-}

-- TODO: use `MVar` or `TMVar` for the parser to avoid concurrent use
type ParserEnv :: Type -> Type
data ParserEnv uri = ParserEnv
  { parserVar :: IORef Parser
  , parserCacheVar :: IORef (ParserCache uri)
  }

-- | Create an empty type-checker environment.
newParserEnv :: IO (ParserEnv uri)
newParserEnv = do
  parserVar <- newIORef =<< newParser
  parserCacheVar <- newIORef Cache.empty
  pure $ ParserEnv{..}
