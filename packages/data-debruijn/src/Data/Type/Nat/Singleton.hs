module Data.Type.Nat.Singleton (
  SNat (Z, S),
  fromSNat,
  fromSNatRaw,
  decSNat,
  SomeSNat (..),
  withSomeSNat,
  toSomeSNat,
  fromSomeSNat,
) where

import Data.Type.Nat.Singleton.Unsafe (
  SNat (S, Z),
  SomeSNat (..),
  decSNat,
  fromSNat,
  fromSNatRaw,
  fromSomeSNat,
  toSomeSNat,
  withSomeSNat,
 )
