{-# LANGUAGE ExplicitNamespaces #-}

module Data.Thinning (
  (:<) (Done, Keep, Drop),
  toBits,
  Thin (..),
) where

import Data.Thinning.Unsafe (
  Thin (..),
  toBits,
  type (:<) (Done, Drop, Keep),
 )
