{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.DeBruijn where

import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

-- | Type-level natural numbers.
type data Nat = Z | S Nat

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

{-| @'SNatF'@ is the base functor of @'SNat'@.
data SNatF (snat :: Nat -> Type) (n :: Nat) :: Type where
  SZF :: (n ~ Z) => SNatF snat n
  SSF :: (n ~ S m) => snat m -> SNatF snat n
-}

-- | @'Ix' n@ is the type of natural numbers less than @n@.
newtype Ix (n :: Nat) = UnsafeIx Word

deriving instance Eq (Ix n)

deriving instance Ord (Ix n)

instance Show (Ix n) where
  show :: Ix n -> String
  show (UnsafeIx u) = show u

type role Ix nominal

-- | Convert an 'Ix' to 'Word'.
toWord :: Ix n -> Word
toWord (UnsafeIx u) = u

-- | @'IxF'@ is the base functor of @'Ix'@.
data IxF (ix :: Nat -> Type) (n :: Nat) :: Type where
  FZF :: IxF ix (S m)
  FSF :: !(ix m) -> IxF ix (S m)

sucIx :: Ix n -> Ix (S n)
sucIx (UnsafeIx u) = UnsafeIx (u + 1)
{-# INLINE sucIx #-}

projectIx :: Ix n -> IxF Ix n
projectIx (UnsafeIx u) =
  unsafeCoerce $
    if u == 0
      then FZF
      else FSF (UnsafeIx (u - 1))
{-# INLINE projectIx #-}

embedIx :: IxF Ix (S n) -> Ix (S n)
embedIx FZF = UnsafeIx 0
embedIx (FSF i) = sucIx i
{-# INLINE embedIx #-}

-- TODO:
-- Type signatures for pattern synonyms are weird.
-- We may be able to use that to simplify this code?
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html#typing-of-pattern-synonyms

pattern FZ :: () => (n ~ S (Pred n)) => Ix n
pattern FZ <- (projectIx -> FZF)
  where
    FZ = embedIx FZF
{-# INLINE FZ #-}

pattern FS :: () => (n ~ S (Pred n)) => Ix (Pred n) -> Ix n
pattern FS i <- (projectIx -> FSF i)
  where
    FS i = embedIx (FSF i)
{-# INLINE FS #-}

{-# COMPLETE FZ, FS #-}

-- | If any value of type @'Ix' n@ exists, @n@ must have a predecessor.
hasPred :: Ix n -> ((n ~ S (Pred n)) => a) -> a
hasPred FZ r = r
hasPred (FS _) r = r

-- | Thinning.
thin :: Ix (S n) -> Ix n -> Ix (S n)
thin FZ j = FS j
thin (FS _) FZ = FZ
thin (FS i) (FS j) = FS (thin i j)

-- | Thickening.
thick :: Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick FZ FZ = Nothing
thick FZ (FS j) = Just j
thick (FS i) FZ = hasPred i $ Just FZ
thick (FS i) (FS j) = hasPred i $ FS <$> thick i j

-- -- | Inject.
-- inject :: Proxy n -> Ix m -> Ix (n + m)
-- inject _ = unsafeCoerce

-- {-| Raise.
-- raise :: SNat n -> Ix m -> Ix (n + m)
-- raise  SZ    i = i
-- raise (SS n) i = FS (raise n i)
-- -}
