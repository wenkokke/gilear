{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.DeBruijn (
  -- * Type-level natural numbers
  Nat (..),
  type Pred,
  type (+),

  -- * De Bruijn Indices
  Ix (FZ, FS),
  toWord,
  thin,
  thick,
  inject,
) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- | Type-level natural numbers.
data Nat = Z | S Nat

-- | Addition of type-level naturals.
type family (+) (n :: Nat) (m :: Nat) = (r :: Nat) where
  Z + m = m
  S n + m = S (n + m)

-- | Predecessor of type-level naturals.
type family Pred (n :: Nat) = (r :: Nat) where
  Pred (S n) = n

{-| @'SNat'@ is the singleton type for natural numbers.
newtype SNat (n :: Nat) = UnsafeSN {unSN :: Word}
type role SNat nominal
-}

-- | @'Ix' n@ is the type of natural numbers less than @n@.
newtype Ix (n :: Nat) = UnsafeIx {unIx :: Word}

type role Ix nominal

-- | Convert an 'Ix' to 'Word'.
toWord :: Ix n -> Word
toWord = unIx

-- | @'IxF'@ is the base functor of @'Ix'@.
data IxF (ix :: Nat -> Type) (n :: Nat) :: Type where
  FZF :: (S m ~ n) => IxF ix n
  FSF :: (S m ~ n) => ix m -> IxF ix n

-- | @'Dict' c@ is the type that holds the evidence for constraint @c@.
data Dict (c :: Constraint) :: Type where
  Dict :: (c) => Dict c

unsafeHasPred :: forall n. Dict (S (Pred n) ~ n)
unsafeHasPred = unsafeCoerce (Dict @(n ~ n))

unsafeWithHasPred :: forall n f. f (S (Pred n)) -> f n
unsafeWithHasPred i = case unsafeHasPred @n of Dict -> i

suc :: Ix (Pred n) -> Ix n
suc (UnsafeIx index) = UnsafeIx (index + 1)

project :: Ix n -> IxF Ix n
project (UnsafeIx index) =
  unsafeWithHasPred $
    if index == 0
      then FZF
      else FSF (UnsafeIx (index - 1))

embed :: IxF Ix (S (Pred n)) -> Ix n
embed FZF = UnsafeIx 0
embed (FSF i) = suc i

pattern FZ :: forall n. Ix n
pattern FZ <- (project -> FZF)
  where
    FZ = embed FZF
{-# INLINE FZ #-}

pattern FS :: forall n. Ix (Pred n) -> Ix n
pattern FS i <- (project -> FSF i)
  where
    FS i = embed (FSF i)
{-# INLINE FS #-}

{-# COMPLETE FZ, FS #-}

-- | Thinning.
thin :: Ix (S n) -> Ix n -> Ix (S n)
thin = thin'
 where
  thin' :: Ix n -> Ix (Pred n) -> Ix n
  thin' FZ j = FS j
  thin' (FS _) FZ = FZ
  thin' (FS i) (FS j) = FS (thin' i j)

-- | Thickening.
thick :: Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick = thick'
 where
  thick' :: Ix n -> Ix n -> Maybe (Ix (Pred n))
  thick' FZ FZ = Nothing
  thick' FZ (FS j) = Just j
  thick' (FS _) FZ = Just FZ
  thick' (FS i) (FS j) = FS <$> thick' i j

-- | Inject.
inject :: Proxy n -> Ix m -> Ix (n + m)
inject _ = unsafeCoerce

{-| Raise.
raise :: SNat n -> Ix m -> Ix (n + m)
raise  SZ    i = i
raise (SS n) i = FS (raise n i)
-}
