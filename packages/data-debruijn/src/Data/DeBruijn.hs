{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}

module Data.DeBruijn (
  Ix (FZ, FS),
  toWord,
  isPos,
  thin,
  thick,
) where

import Data.Kind (Type)
import Data.Type.Nat (Pos, Pred, type Nat (..))
import Data.Word (Word16)
import Unsafe.Coerce (unsafeCoerce)

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
newtype Ix (n :: Nat) = UnsafeIx Word16

deriving instance Eq (Ix n)

deriving instance Ord (Ix n)

instance Show (Ix n) where
  show :: Ix n -> String
  show (UnsafeIx u) = show u

type role Ix nominal

-- | Convert an 'Ix' to 'Word'.
toWord :: Ix n -> Word16
toWord (UnsafeIx u) = u
{-# INLINE toWord #-}

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

-- NOTE:
--   Type signatures for pattern synonyms are weird, see:
--   https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html#typing-of-pattern-synonyms

pattern FZ :: () => (Pos n) => Ix n
pattern FZ <- (projectIx -> FZF) where FZ = embedIx FZF
{-# INLINE FZ #-}

pattern FS :: () => (Pos n) => Ix (Pred n) -> Ix n
pattern FS i <- (projectIx -> FSF i) where FS i = embedIx (FSF i)
{-# INLINE FS #-}

{-# COMPLETE FZ, FS #-}

-- | If any value of type @'Ix' n@ exists, @n@ must have a predecessor.
isPos :: Ix n -> ((Pos n) => a) -> a
isPos FZ r = r
isPos (FS _) r = r

-- | Thinning.
thin :: Ix (S n) -> Ix n -> Ix (S n)
thin FZ j = FS j
thin (FS _) FZ = FZ
thin (FS i) (FS j) = FS (thin i j)

-- | Thickening.
thick :: Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick FZ FZ = Nothing
thick FZ (FS j) = Just j
thick (FS i) FZ = isPos i $ Just FZ
thick (FS i) (FS j) = isPos i $ FS <$> thick i j

-- -- | Inject.
-- inject :: Proxy n -> Ix m -> Ix (n + m)
-- inject _ = unsafeCoerce

-- {-| Raise.
-- raise :: SNat n -> Ix m -> Ix (n + m)
-- raise  SZ    i = i
-- raise (SS n) i = FS (raise n i)
-- -}
