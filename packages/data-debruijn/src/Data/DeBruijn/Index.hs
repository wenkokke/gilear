module Data.DeBruijn.Index (
  -- * DeBruijn Indexes
  Ix (FZ, FS),
  eqIx,
  fromIx,
  fromIxRaw,
  isPos,
  thin,
  thick,
  inject,
  raise,

  -- * Existential Wrapper
  SomeIx (..),
  withSomeIx,
  toSomeIx,
  toSomeIxRaw,
  fromSomeIx,
  fromSomeIxRaw,
) where

import Data.DeBruijn.Index.Unsafe (
  Ix (FS, FZ),
  SomeIx (..),
  eqIx,
  fromIx,
  fromIxRaw,
  fromSomeIx,
  fromSomeIxRaw,
  inject,
  isPos,
  raise,
  thick,
  thin,
  toSomeIx,
  toSomeIxRaw,
  withSomeIx,
 )
