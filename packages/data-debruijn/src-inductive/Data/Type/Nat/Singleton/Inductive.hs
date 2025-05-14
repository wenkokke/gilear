{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Type.Nat.Singleton.Inductive (
  -- * Natural Number Singletons
  SNat (..),
  toInductive,
  fromInductive,
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
) where

import Control.DeepSeq (NFData (..))
import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Equality qualified as Eq
import Data.Type.Nat (Nat (..), type (+))
import Data.Type.Nat.Singleton qualified as Efficient

{- $setup
>>> import Data.Type.Nat.Singleton.Inductive.Arbitrary
-}

--------------------------------------------------------------------------------
-- Natural Number Singletons
--------------------------------------------------------------------------------

-- | @'SNat' n@ is the singleton type for natural numbers.
type SNat :: Nat -> Type
data SNat n where
  Z :: SNat Z
  S :: SNat n -> SNat (S n)

instance NFData (SNat n) where
  rnf :: SNat n -> ()
  rnf Z = ()
  rnf (S n) = rnf n

-- | Convert from the efficient representation 'Efficient.SNat' to the inductive representation 'SNat'.
toInductive :: Efficient.SNat n -> SNat n
toInductive Efficient.Z = Z
toInductive (Efficient.S n) = S (toInductive n)

-- | Convert from the inductive representation 'SNat' to the efficient representation 'Efficient.SNat'.
fromInductive :: SNat n -> Efficient.SNat n
fromInductive Z = Efficient.Z
fromInductive (S n) = Efficient.S (fromInductive n)

instance Show (SNat n) where
  showsPrec :: Int -> SNat n -> ShowS
  showsPrec p =
    showParen (p > 10) . \case
      Z -> showString "Z"
      S n -> showString "S " . showsPrec 11 n

-- | @'fromSNat' n@ returns the numeric representation of 'SNat n'.
{-# SPECIALIZE fromSNat :: SNat n -> Int #-}
fromSNat :: (Integral i) => SNat n -> i
fromSNat Z = 0
fromSNat (S n') = succ (fromSNat n')

fromSNatRaw :: SNat n -> Int
fromSNatRaw = fromSNat
{-# INLINE fromSNatRaw #-}

-- | Decidable equality for natural number singletons.
decSNat :: SNat m -> SNat n -> Maybe (m :~: n)
decSNat Z Z = Just Refl
decSNat (S m') (S n') = (\Refl -> Refl) <$> decSNat m' n'
decSNat _m _n = Nothing

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
toSomeSNat n = iterate' n (withSomeSNat $ SomeSNat . S) (SomeSNat Z)

{-| @'toSomeSNat' n@ constructs the singleton @'SNat' n@.

prop> toSomeSNatRaw (fromSomeSNatRaw n) == n
-}
toSomeSNatRaw :: Int -> SomeSNat
toSomeSNatRaw = toSomeSNat

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped singleton.
fromSomeSNat :: (Integral i) => SomeSNat -> i
fromSomeSNat = withSomeSNat fromSNat

-- | @'fromSomeSNat' n@ returns the 'Int' representation of the wrapped singleton.
fromSomeSNatRaw :: SomeSNat -> Int
fromSomeSNatRaw = fromSomeSNat

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

plusUnitL :: Proxy n -> Z + n :~: n
plusUnitL _ = Refl

plusUnitR :: SNat n -> n + Z :~: n
plusUnitR Z = Refl
plusUnitR (S n') =
  case plusUnitR n' of
    Refl -> Refl

plusCommS :: SNat n -> Proxy m -> S (n + m) :~: n + S m
plusCommS Z _ = Refl
plusCommS (S n') m = Eq.apply Refl (plusCommS n' m)

plusComm :: SNat n -> SNat m -> n + m :~: m + n
plusComm Z m = Eq.sym (plusUnitR m)
plusComm (S n') m = Eq.apply Refl (plusComm n' m) `Eq.trans` plusCommS m (erase n')

plusAssoc :: SNat n -> Proxy m -> Proxy l -> (n + m) + l :~: n + (m + l)
plusAssoc Z _m _l = Refl
plusAssoc (S n') m l = Eq.apply Refl (plusAssoc n' m l)

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

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | @`erase` x@ erases the content of @x@ to a @`Proxy`@.
erase :: f a -> Proxy a
erase _ = Proxy
{-# INLINE erase #-}

-- | @`iterate'` i f@ applies @f@ @i@ times.
iterate' :: (Integral i) => i -> (a -> a) -> a -> a
iterate' i f x
  | i == 0 = x
  | otherwise = iterate' (i - 1) f $! f x
