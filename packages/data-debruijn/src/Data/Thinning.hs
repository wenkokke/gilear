{-# LANGUAGE ExplicitNamespaces #-}

module Data.Thinning (
  (:<) (Done, Keep, Drop),
  toBools,
  Thin (..),
) where

import Data.Thinning.Unsafe (
  Thin (..),
  toBools,
  type (:<) (Done, Drop, Keep),
 )
