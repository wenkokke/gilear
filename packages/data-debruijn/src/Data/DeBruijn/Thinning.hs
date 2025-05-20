{-# LANGUAGE ExplicitNamespaces #-}

module Data.DeBruijn.Thinning (
  (:<=) (Done, Keep, Drop),
  toBools,
  Thin (..),
) where

import Data.DeBruijn.Thinning.Unsafe (
  Thin (..),
  toBools,
  type (:<=) (Done, Drop, Keep),
 )
