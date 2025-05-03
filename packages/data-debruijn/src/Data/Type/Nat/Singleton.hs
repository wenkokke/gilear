module Data.Type.Nat.Singleton (
  -- * Natural Number Singletons
  SNat (Z, S),
  fromSNat,
  fromSNatRaw,
  decSNat,

  -- * Existential Wrapper
  SomeSNat (..),
  withSomeSNat,
  toSomeSNat,
  toSomeSNatRaw,
  fromSomeSNat,
  fromSomeSNatRaw,
) where

import Data.Type.Nat.Singleton.Unsafe (
  SNat (S, Z),
  SomeSNat (..),
  decSNat,
  fromSNat,
  fromSNatRaw,
  fromSomeSNat,
  fromSomeSNatRaw,
  toSomeSNat,
  toSomeSNatRaw,
  withSomeSNat,
 )
