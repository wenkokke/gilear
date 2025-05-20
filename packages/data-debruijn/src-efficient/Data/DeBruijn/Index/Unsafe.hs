{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.DeBruijn.Index.Unsafe (
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

  -- * Unsafe
  Ix (UnsafeIx),
) where

import Control.DeepSeq (NFData (..))
import Control.Exception (assert)
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat (Nat (..), Pos, Pred, type (+))
import Data.Type.Nat.Singleton.Unsafe (SNat (..), decSNat)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

{- $setup
>>> import Data.DeBruijn.Index.Arbitrary
-}

--------------------------------------------------------------------------------
-- DeBruijn Index Representation
--------------------------------------------------------------------------------

#define IxRep Int

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

thinRep :: IxRep -> IxRep -> IxRep
thinRep i j
  | i <= j = succ j
  | otherwise = j

thickRep :: IxRep -> IxRep -> Maybe IxRep
thickRep i j = case i `compare` j of
  LT -> Just (pred j)
  EQ -> Nothing
  GT -> Just j

--------------------------------------------------------------------------------
-- DeBruijn Indexes
--------------------------------------------------------------------------------

-- | @'Ix' n@ is the type of DeBruijn indices less than @n@.
type Ix :: Nat -> Type
newtype Ix n = UnsafeIx {getIxRep :: IxRep}

type role Ix nominal

eqIx :: Ix n -> Ix m -> Bool
eqIx i j = fromIxRaw i == fromIxRaw j

instance Eq (Ix n) where
  (==) :: Ix n -> Ix n -> Bool
  (==) = eqIx

instance Show (Ix n) where
  showsPrec :: Int -> Ix n -> ShowS
  showsPrec p =
    showParen (p > 10) . \case
      FZ -> showString "FZ"
      FS n -> showString "FS " . showsPrec 11 n

instance NFData (Ix n) where
  rnf :: Ix n -> ()
  rnf (UnsafeIx u) = rnf u

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
fromIxRaw (UnsafeIx w) = w
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
thin (UnsafeIx i) (UnsafeIx j) = UnsafeIx (thinRep i j)

-- | Thickening.
thick :: Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick (UnsafeIx i) (UnsafeIx j) = UnsafeIx <$> thickRep i j

-- | Inject.
inject :: Proxy n -> Ix m -> Ix (n + m)
inject _ (UnsafeIx j) = UnsafeIx j

-- | Raise.
raise :: SNat n -> Ix m -> Ix (n + m)
raise (UnsafeSNat n) (UnsafeIx j) = UnsafeIx (n + j)

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

-- | An existential wrapper around indexes.
type SomeIx :: Type
data SomeIx = forall (n :: Nat). SomeIx
  { bound :: {-# UNPACK #-} !(SNat n)
  , index :: {-# UNPACK #-} !(Ix n)
  }

instance NFData SomeIx where
  rnf :: SomeIx -> ()
  rnf SomeIx{..} = rnf bound `seq` rnf index

instance Eq SomeIx where
  (==) :: SomeIx -> SomeIx -> Bool
  SomeIx n i == SomeIx m j
    | Just Refl <- decSNat n m = eqIx i j
    | otherwise = False

deriving instance Show SomeIx

withSomeIx :: (forall n. SNat n -> Ix n -> a) -> SomeIx -> a
withSomeIx action (SomeIx n i) = action n i

{-| @'toSomeIx' n@ constructs the index @n@ at type @'Ix' n@ from the number @n@.

prop> toSomeIx (fromSomeIx i) == i
-}
toSomeIx :: (Integral i) => (i, i) -> SomeIx
toSomeIx = toSomeIxRaw . bimap fromIntegral fromIntegral

{-| @'toSomeIxRaw' n@ constructs the index @n@ at type @'Ix' n@ from the 'Int' @n@.

prop> toSomeIxRaw (fromSomeIxRaw i) == i
-}
toSomeIxRaw :: (IxRep, IxRep) -> SomeIx
toSomeIxRaw (bound, index)
  | index < 0 = error $ printf "index cannot contain negative value, found index %d" (toInteger index)
  | bound <= index = error $ printf "bound must be larger than index, found bound %d and index %d" (toInteger bound) (toInteger index)
  | otherwise = SomeIx (UnsafeSNat bound) (UnsafeIx index)

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped index.
fromSomeIx :: (Integral i) => SomeIx -> (i, i)
fromSomeIx = bimap fromIntegral fromIntegral . fromSomeIxRaw

-- | @'fromSomeSNat' n@ returns the 'Int' representation of the wrapped index.
fromSomeIxRaw :: SomeIx -> (IxRep, IxRep)
fromSomeIxRaw (SomeIx (UnsafeSNat bound) (UnsafeIx index)) = (bound, index)
