{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.DeBruijn.Level.Unsafe (
  -- * DeBruijn Indexes
  Lv (LZ, LS),
  eqLv,
  fromLv,
  fromLvRaw,
  isPos,
  thin,
  thick,
  inject,
  raise,

  -- * Existential Wrapper
  SomeLv (..),
  withSomeLv,
  toSomeLv,
  toSomeLvRaw,
  fromSomeLv,
  fromSomeLvRaw,

  -- * Unsafe
  Lv (UnsafeLv),
) where

import Control.Exception (assert)
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat (Nat (..), Pos, Pred, type (+))
import Data.Type.Nat.Singleton.Unsafe (SNat (..), decSNat)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- DeBruijn Index Representation
--------------------------------------------------------------------------------

#define LvRep Int

mkLZRep :: LvRep
mkLZRep = 0
{-# INLINE mkLZRep #-}

mkLSRep :: LvRep -> LvRep
mkLSRep = succ
{-# INLINE mkLSRep #-}

getLvRepChild :: LvRep -> LvRep
getLvRepChild r =
  assert (r /= mkLZRep) $
    pred r
{-# INLINE getLvRepChild #-}

recLvRep :: LvRep -> a -> (LvRep -> a) -> a
recLvRep r ifZ ifS = if r == mkLZRep then ifZ else ifS (getLvRepChild r)
{-# INLINE recLvRep #-}

--------------------------------------------------------------------------------
-- DeBruijn Levels
--------------------------------------------------------------------------------

-- | @'Lv' n@ is the type of DeBruijn levels less than @n@.
type Lv :: Nat -> Type
newtype Lv n = UnsafeLv {getLvRep :: LvRep}

type role Lv nominal

eqLv :: Lv n -> Lv m -> Bool
eqLv i j = fromLvRaw i == fromLvRaw j

instance Show (Lv n) where
  showsPrec :: Int -> Lv n -> ShowS
  showsPrec p =
    showParen (p > 10) . \case
      LZ -> showString "LZ"
      LS n -> showString "LS " . showsPrec 11 n

mkLZ :: Lv (S n)
mkLZ = UnsafeLv mkLZRep
{-# INLINE mkLZ #-}

mkLS :: Lv n -> Lv (S n)
mkLS = UnsafeLv . mkLSRep . getLvRep
{-# INLINE mkLS #-}

recLv :: Lv n -> a -> (Lv (Pred n) -> a) -> a
recLv r ifLZ ifLS = recLvRep (getLvRep r) ifLZ (ifLS . UnsafeLv)
{-# INLINE recLv #-}

-- | @'fromSNat' n@ returns the numeric representation of 'SNat n'.
fromLv :: (Integral i) => Lv n -> i
fromLv (UnsafeLv u) = fromInteger (toInteger u)
{-# INLINE fromLv #-}

-- | @'fromLvRaw' n@ returns the raw numeric representation of 'SNat n'.
fromLvRaw :: Lv n -> Int
fromLvRaw (UnsafeLv w) = w
{-# INLINE fromLvRaw #-}

-- | @'LvF'@ is the base functor of @'Lv'@.
data LvF (ix :: Nat -> Type) (n :: Nat) :: Type where
  LZF :: LvF ix (S m)
  LSF :: !(ix m) -> LvF ix (S m)

projectLv :: Lv n -> LvF Lv n
projectLv i = recLv i (unsafeCoerce LZF) (unsafeCoerce . LSF)
{-# INLINE projectLv #-}

embedLv :: LvF Lv n -> Lv n
embedLv = \case
  LZF -> mkLZ
  LSF i -> mkLS i
{-# INLINE embedLv #-}

-- NOTE:
--   Type signatures for pattern synonyms are weird, see:
--   https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html#typing-of-pattern-synonyms

pattern LZ :: () => (Pos n) => Lv n
pattern LZ <- (projectLv -> LZF) where LZ = embedLv LZF
{-# INLINE LZ #-}

pattern LS :: () => (Pos n) => Lv (Pred n) -> Lv n
pattern LS i <- (projectLv -> LSF i) where LS i = embedLv (LSF i)
{-# INLINE LS #-}

{-# COMPLETE LZ, LS #-}

-- | If any value of type @'Lv' n@ exists, @n@ must have a predecessor.
isPos :: Lv n -> ((Pos n) => a) -> a
isPos LZ r = r
isPos (LS _) r = r

-- | Thinning.
thin :: Lv (S n) -> Lv n -> Lv (S n)
thin LZ j = LS j
thin (LS _) LZ = LZ
thin (LS i) (LS j) = LS (thin i j)

-- | Thickening.
thick :: Lv (S n) -> Lv (S n) -> Maybe (Lv n)
thick LZ LZ = Nothing
thick LZ (LS j) = Just j
thick (LS i) LZ = isPos i $ Just LZ
thick (LS i) (LS j) = isPos i $ LS <$> thick i j

-- | Inject.
inject :: Proxy n -> Lv m -> Lv (n + m)
inject _ (UnsafeLv j) = UnsafeLv j

-- | Raise.
raise :: SNat n -> Lv m -> Lv (n + m)
raise (UnsafeSNat n) (UnsafeLv j) = UnsafeLv (n + j)

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

-- | An existential wrapper around levels.
type SomeLv :: Type
data SomeLv = forall (n :: Nat). SomeLv
  { bound :: {-# UNPACK #-} !(SNat n)
  , index :: {-# UNPACK #-} !(Lv n)
  }

instance Eq SomeLv where
  (==) :: SomeLv -> SomeLv -> Bool
  SomeLv n i == SomeLv m j
    | Just Refl <- decSNat n m = eqLv i j
    | otherwise = False

deriving instance Show SomeLv

withSomeLv :: (forall n. SNat n -> Lv n -> a) -> SomeLv -> a
withSomeLv action (SomeLv n i) = action n i

{-| @'toSomeLv' n@ constructs the level @n@ at type @'Lv' n@ from the number @n@.

@
toSomeLv (fromSomeLv i) == i
@
-}
toSomeLv :: (Integral i) => (i, i) -> SomeLv
toSomeLv = toSomeLvRaw . bimap fromIntegral fromIntegral

{-| @'toSomeLvRaw' n@ constructs the index @n@ at type @'Lv' n@ from the 'Int' @n@.

@
toSomeLvRaw (fromSomeLvRaw i) == i
@
-}
toSomeLvRaw :: (LvRep, LvRep) -> SomeLv
toSomeLvRaw (bound, index)
  | index < 0 = error $ printf "index cannot contain negative value, found index %d" (toInteger index)
  | bound <= index = error "bound must be larger than index, found bound %d and index %d" (toInteger bound) (toInteger index)
  | otherwise = SomeLv (UnsafeSNat bound) (UnsafeLv index)

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped index.
fromSomeLv :: (Integral i) => SomeLv -> (i, i)
fromSomeLv = bimap fromIntegral fromIntegral . fromSomeLvRaw

-- | @'fromSomeSNat' n@ returns the 'Int' representation of the wrapped index.
fromSomeLvRaw :: SomeLv -> (LvRep, LvRep)
fromSomeLvRaw (SomeLv (UnsafeSNat bound) (UnsafeLv index)) = (bound, index)
