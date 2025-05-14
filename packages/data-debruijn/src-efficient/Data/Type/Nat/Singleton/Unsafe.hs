{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.Type.Nat.Singleton.Unsafe (
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

  -- * Laws
  plusUnitL,
  plusUnitR,
  plusCommS,
  plusComm,
  plusAssoc,

  -- * Induction Principles
  withInstance,

  -- * Unsafe
  SNat (UnsafeSNat, sNatRep),
) where

import Control.DeepSeq (NFData (..))
import Control.Exception (assert)
import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Nat (Nat (..), Pos, Pred, type (+))
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

{- $setup
>>> import Data.Type.Nat.Singleton.Arbitrary
-}

--------------------------------------------------------------------------------
-- Natural Number Singleton Representation
--------------------------------------------------------------------------------

#define SNatRep Int

isValidSNatRep :: SNatRep -> Bool
isValidSNatRep u = u >= 0

mkZRep :: SNatRep
mkZRep = 0
{-# INLINE mkZRep #-}

mkSRep :: SNatRep -> SNatRep
mkSRep u =
  assert (isValidSNatRep u) $
    succ u
{-# INLINE mkSRep #-}

getSNatRepChild :: SNatRep -> SNatRep
getSNatRepChild u =
  assert (isValidSNatRep u && u /= mkZRep) $
    pred u
{-# INLINE getSNatRepChild #-}

elSNatRep :: SNatRep -> a -> (SNatRep -> a) -> a
elSNatRep u ifZ ifS =
  assert (isValidSNatRep u) $
    if u == mkZRep
      then ifZ
      else ifS (getSNatRepChild u)
{-# INLINE elSNatRep #-}

--------------------------------------------------------------------------------
-- Natural Number Singletons
--------------------------------------------------------------------------------

-- | @'SNat' n@ is the singleton type for natural numbers.
type SNat :: Nat -> Type
newtype SNat n = UnsafeSNat {sNatRep :: SNatRep}

instance NFData (SNat n) where
  rnf :: SNat n -> ()
  rnf (UnsafeSNat u) = rnf u

type role SNat nominal

mkZ :: SNat Z
mkZ = UnsafeSNat mkZRep
{-# INLINE mkZ #-}

mkS :: SNat n -> SNat (S n)
mkS = UnsafeSNat . mkSRep . sNatRep
{-# INLINE mkS #-}

-- | @'fromSNat' n@ returns the numeric representation of 'SNat n'.
fromSNat :: (Integral i) => SNat n -> i
fromSNat (UnsafeSNat u) = fromInteger (toInteger u)

-- | @'fromSNatRaw' n@ returns the raw underlying representation of 'SNat n'.
fromSNatRaw :: SNat n -> Int
fromSNatRaw (UnsafeSNat w) = w

instance Show (SNat n) where
  showsPrec :: Int -> SNat n -> ShowS
  showsPrec p =
    showParen (p > 10) . \case
      Z -> showString "Z"
      S n -> showString "S " . showsPrec 11 n

-- | @'SNatF'@ is the base functor of @'SNat'@.
data SNatF (snat :: Nat -> Type) (n :: Nat) where
  ZF :: SNatF snat Z
  SF :: !(snat n) -> SNatF snat (S n)

projectSNat :: SNat n -> SNatF SNat n
projectSNat (UnsafeSNat u) = elSNatRep u (unsafeCoerce ZF) (unsafeCoerce . SF . UnsafeSNat)
{-# INLINE projectSNat #-}

embedSNat :: SNatF SNat n -> SNat n
embedSNat = \case
  ZF -> mkZ
  SF n -> mkS n
{-# INLINE embedSNat #-}

pattern Z :: () => (n ~ Z) => SNat n
pattern Z <- (projectSNat -> ZF) where Z = embedSNat ZF
{-# INLINE Z #-}

pattern S :: () => (Pos n) => SNat (Pred n) -> SNat n
pattern S n <- (projectSNat -> SF n) where S n = embedSNat (SF n)
{-# INLINE S #-}

{-# COMPLETE Z, S #-}

-- | Decidable equality for natural number singletons.
decSNat :: SNat m -> SNat n -> Maybe (m :~: n)
decSNat (UnsafeSNat u1) (UnsafeSNat u2) =
  if u1 == u2
    then Just (unsafeCoerce Refl)
    else Nothing

instance Eq (SNat n) where
  (==) :: SNat n -> SNat n -> Bool
  m == n = isJust (decSNat m n)

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

-- | An existential wrapper around natural number singletons.
type SomeSNat :: Type
data SomeSNat = forall (n :: Nat). SomeSNat !(SNat n)

instance NFData SomeSNat where
  rnf :: SomeSNat -> ()
  rnf (SomeSNat n) = rnf n

deriving instance Show SomeSNat

instance Eq SomeSNat where
  (==) :: SomeSNat -> SomeSNat -> Bool
  SomeSNat m == SomeSNat n = isJust (decSNat m n)

-- | Evaluate a term with access to the underlying @'SNat'@.
withSomeSNat :: (forall n. SNat n -> a) -> SomeSNat -> a
withSomeSNat action (SomeSNat n) = action n

{-| @'toSomeSNat' n@ constructs the singleton @'SNat' n@.

prop> toSomeSNat (fromSomeSNat n) == n
-}
toSomeSNat :: (Integral i) => i -> SomeSNat
toSomeSNat u
  | u < 0 = error $ printf "cannot convert %d to natural number singleton" (toInteger u)
  | otherwise = SomeSNat (UnsafeSNat (fromIntegral u))

{-| @'toSomeSNat' n@ constructs the singleton @'SNat' n@.

prop> toSomeSNatRaw (fromSomeSNatRaw n) == n
-}
toSomeSNatRaw :: Int -> SomeSNat
toSomeSNatRaw u
  | u < 0 = error $ printf "cannot convert %d to natural number singleton"
  | otherwise = SomeSNat (UnsafeSNat u)

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped singleton.
fromSomeSNat :: (Integral i) => SomeSNat -> i
fromSomeSNat = withSomeSNat fromSNat

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped singleton.
fromSomeSNatRaw :: SomeSNat -> Int
fromSomeSNatRaw (SomeSNat (UnsafeSNat u)) = u

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

plusUnitL :: Proxy n -> Z + n :~: n
plusUnitL _ = Refl

plusUnitR :: SNat n -> n + Z :~: n
plusUnitR _ = unsafeCoerce Refl

plusCommS :: SNat n -> Proxy m -> S (n + m) :~: n + S m
plusCommS _ _ = unsafeCoerce Refl

plusComm :: SNat n -> SNat m -> n + m :~: m + n
plusComm _ _ = unsafeCoerce Refl

plusAssoc :: SNat n -> Proxy m -> Proxy l -> (n + m) + l :~: n + (m + l)
plusAssoc _ _ _ = unsafeCoerce Refl

--------------------------------------------------------------------------------
-- Induction Principles
--------------------------------------------------------------------------------

withInstance ::
  forall (c :: Nat -> Constraint).
  (c Z, forall n. c (S n)) =>
  forall (n :: Nat) (r :: Type).
  SNat n ->
  ((c n) => r) ->
  r
withInstance Z action = action
withInstance (S n) action = withInstance @c n action
