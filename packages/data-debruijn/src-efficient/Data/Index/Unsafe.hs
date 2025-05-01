{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.Index.Unsafe (
  Ix (FZ, FS),
  fromIx,
  fromIxRaw,
  isPos,
  thin,
  thick,
  inject,
  raise,

  -- * Unsafe
  Ix (UnsafeIx),
  IxRep (IxRep, ixRepRaw),
) where

import Control.Exception (assert)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Nat (Nat (..), Pos, Pred, type (+))
import Data.Type.Nat.Singleton.Unsafe (SNat (..), SNatRep (..))
import Unsafe.Coerce (unsafeCoerce)

newtype IxRep = IxRep {ixRepRaw :: Int}
  deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

mkFZRep :: IxRep
mkFZRep = 0
{-# INLINE mkFZRep #-}

mkFSRep :: IxRep -> IxRep
mkFSRep = succ
{-# INLINE mkFSRep #-}

getIxRepChild :: IxRep -> IxRep
getIxRepChild r =
  assert (r /= mkFZRep) $
    pred r
{-# INLINE getIxRepChild #-}

recIxRep :: IxRep -> a -> (IxRep -> a) -> a
recIxRep r ifZ ifS = if r == mkFZRep then ifZ else ifS (getIxRepChild r)
{-# INLINE recIxRep #-}

-- | @'Ix' n@ is the type of DeBruijn indices less than @n@.
type Ix :: Nat -> Type
newtype Ix n = UnsafeIx {getIxRep :: IxRep}

type role Ix nominal

instance Show (Ix n) where
  showsPrec :: Int -> Ix n -> ShowS
  showsPrec p = \case
    FZ -> showString "FZ"
    FS n -> showString "FS " . showParen (p > 10) (showsPrec 11 n)

mkFZ :: Ix (S n)
mkFZ = UnsafeIx mkFZRep
{-# INLINE mkFZ #-}

mkFS :: Ix n -> Ix (S n)
mkFS = UnsafeIx . mkFSRep . getIxRep
{-# INLINE mkFS #-}

recIx :: Ix n -> a -> (Ix (Pred n) -> a) -> a
recIx r ifFZ ifFS = recIxRep (getIxRep r) ifFZ (ifFS . UnsafeIx)
{-# INLINE recIx #-}

-- | @'fromSNat' n@ returns the numeric representation of 'SNat n'.
fromIx :: (Integral i) => Ix n -> i
fromIx (UnsafeIx u) = fromInteger (toInteger u)
{-# INLINE fromIx #-}

-- | @'fromIxRaw' n@ returns the raw numeric representation of 'SNat n'.
fromIxRaw :: Ix n -> Int
fromIxRaw (UnsafeIx (IxRep w)) = w
{-# INLINE fromIxRaw #-}

-- | @'IxF'@ is the base functor of @'Ix'@.
data IxF (ix :: Nat -> Type) (n :: Nat) :: Type where
  FZF :: IxF ix (S m)
  FSF :: !(ix m) -> IxF ix (S m)

projectIx :: Ix n -> IxF Ix n
projectIx i = recIx i (unsafeCoerce FZF) (unsafeCoerce . FSF)
{-# INLINE projectIx #-}

embedIx :: IxF Ix n -> Ix n
embedIx = \case
  FZF -> mkFZ
  FSF i -> mkFS i
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

-- | Inject.
inject :: Proxy n -> Ix m -> Ix (n + m)
inject _ (UnsafeIx j) = UnsafeIx j

-- | Raise.
raise :: SNat n -> Ix m -> Ix (n + m)
raise (UnsafeSNat (SNatRep n)) (UnsafeIx (IxRep j)) = UnsafeIx (IxRep (n + j))
