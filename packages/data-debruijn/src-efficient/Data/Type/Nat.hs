{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Type.Nat (
  type Nat (..),
  type (+),
  type Pred,
  type Pos,
  type (<=),
) where

import Data.Kind (Constraint)

-- | Type-level natural numbers.
type data Nat = Z | S Nat

-- | Addition of type-level naturals.
type family (+) (n :: Nat) (m :: Nat) :: Nat where
  Z + m = m
  S n + m = S (n + m)

-- | Predecessor of type-level naturals.
type family Pred (n :: Nat) :: Nat where
  Pred (S n) = n

-- | @'Pos' n@ holds if @n@ is non-zero.
type Pos :: Nat -> Constraint
type Pos (n :: Nat) = n ~ S (Pred n)

-- | Less-than-or-equal for type-level naturals.
type family (<=) (n :: Nat) (m :: Nat) :: Bool where
  Z <= m = 'True
  S n <= Z = 'False
  S n <= S m = n <= m
