{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Data.DeBruijn.Inductive where

import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Nat (type Nat (..), type Pos)
import Numeric.Natural (Natural, minusNaturalMaybe)

{- $setup
>>> import Data.DeBruijn.Inductive.Arbitrary ()
-}

--------------------------------------------------------------------------------
-- Natural Number Singletons
--------------------------------------------------------------------------------

-- | @'SNat' n@ is the singleton type for natural numbers.
type SNat :: Nat -> Type
data SNat n where
  Z :: SNat Z
  S :: !(SNat n) -> SNat (S n)

deriving stock instance Show (SNat n)

-- | Decidable equality for natural number singletons.
decSNat :: SNat m -> SNat n -> Maybe (m :~: n)
decSNat Z Z = Just Refl
decSNat (S m') (S n') = (\Refl -> Refl) <$> decSNat m' n'
decSNat _m _n = Nothing

instance Eq (SNat n) where
  (==) :: SNat n -> SNat n -> Bool
  m == n = isJust (decSNat m n)

-- | @'fromSNat' n@ returns the numeric representation of 'SNat n'.
fromSNat :: SNat n -> Natural
fromSNat Z = 0
fromSNat (S n') = 1 + fromSNat n'

-- | The class of type-level naturals with a known singleton.
type KnownNat :: Nat -> Constraint
class KnownNat n where
  natSing :: SNat n

instance KnownNat Z where
  natSing :: SNat Z
  natSing = Z

instance (KnownNat n) => KnownNat (S n) where
  natSing :: SNat (S n)
  natSing = S natSing

-- | @'withKnownNat' sn x@ constructs the 'KnownNat' instance for @sn@ and makes it available to @x@.
withKnownNat :: SNat n -> ((KnownNat n) => a) -> a
withKnownNat Z x = x
withKnownNat (S n) x = withKnownNat n x

-- | An existential wrapper around natural number singletons.
type SomeSNat :: Type
data SomeSNat = forall (n :: Nat). SomeSNat !(SNat n)

deriving stock instance Show SomeSNat

instance Eq SomeSNat where
  (==) :: SomeSNat -> SomeSNat -> Bool
  SomeSNat m == SomeSNat n = isJust (decSNat m n)

-- | Evaluate a term with access to the underlying @'SNat' n@.
withSomeSNat :: SomeSNat -> (forall n. SNat n -> a) -> a
withSomeSNat (SomeSNat n) action = action n

{-| @'succSomeSNat' sn@ increments the wrapped singleton by one.

prop> succSomeSNat (toSomeSNat n) == toSomeSNat (1 + n)
prop> fromSomeSNat (succSomeSNat n) == 1 + fromSomeSNat n
-}
succSomeSNat :: SomeSNat -> SomeSNat
succSomeSNat (SomeSNat n) = SomeSNat (S n)

{-| @'incrSomeSNat' n sm@ increments the wrapped singleton by @n@.

prop> incrSomeSNat n (toSomeSNat m) == toSomeSNat (n + m)
prop> fromSomeSNat (incrSomeSNat n m) == n + fromSomeSNat m
-}
incrSomeSNat :: Natural -> SomeSNat -> SomeSNat
incrSomeSNat n sm = case n `minusNaturalMaybe` 1 of
  Nothing -> sm
  Just n' -> incrSomeSNat n' (succSomeSNat sm)

{-| @'toSomeSNat' n@ constructs the singleton @'SNat' n@.

prop> fromSomeSNat (toSomeSNat n) == n
prop> toSomeSNat (fromSomeSNat n) == n
-}
toSomeSNat :: Natural -> SomeSNat
toSomeSNat n = incrSomeSNat n (SomeSNat Z)

{-| @'fromSomeSNat' n@ returns the numeric representation of the wrapped singleton.

prop> fromSomeSNat n == withSomeSNat n fromSNat
-}
fromSomeSNat :: SomeSNat -> Natural
fromSomeSNat (SomeSNat n) = fromSNat n

--------------------------------------------------------------------------------
-- De Bruijn Indices
--------------------------------------------------------------------------------

-- | @'Ix' n@ is the type of natural numbers less than @n@.
type Ix :: Nat -> Type
data Ix n where
  FZ :: Ix (S n)
  FS :: !(Ix n) -> Ix (S n)

-- | An existential wrapper around indices.
type SomeIx :: Type
data SomeIx = forall (n :: Nat). SomeIx !(Ix n)

-- | If any value of type @'Ix' n@ exists, @n@ must have a predecessor.
isPos :: Ix n -> ((Pos n) => a) -> a
isPos FZ r = r
isPos (FS _) r = r

--------------------------------------------------------------------------------
-- Thinnings
--------------------------------------------------------------------------------

-- | @'Th' m n@ is the type of thinnings from @m@ to @n@.
type (:<) :: Nat -> Nat -> Type
data (:<) n m where
  Done :: Z :< Z
  Keep :: !(n :< m) -> S n :< S m
  Drop :: !(n :< m) -> n :< S m

-- | The actions of thinnings on natural-indexed data types.
type Thin :: (Nat -> Type) -> Constraint
class Thin f where
  thin :: n :< m -> f n -> f m
  thick :: n :< m -> f m -> Maybe (f n)

instance Thin Ix where
  thin :: n :< m -> Ix n -> Ix m
  thin !t !i = case t of
    Keep n'm' -> case i of
      FZ -> FZ
      FS i' -> FS (thin n'm' i')
    Drop nm' -> FS (thin nm' i)

  thick :: n :< m -> Ix m -> Maybe (Ix n)
  thick Done _i = Nothing
  thick (Keep _n'm') FZ = Just FZ
  thick (Keep n'm') (FS i') = FS <$> thick n'm' i'
  thick (Drop _nm') FZ = Nothing
  thick (Drop nm') (FS i') = thick nm' i'

instance Thin ((:<) l) where
  thin :: n :< m -> l :< n -> l :< m
  thin Done Done = Done
  thin (Keep n'm') (Keep l'n') = Keep (thin n'm' l'n')
  thin (Keep n'm') (Drop ln') = Drop (thin n'm' ln')
  thin (Drop nm') ln = Drop (thin nm' ln)

  thick :: n :< m -> l :< m -> Maybe (l :< n)
  thick Done Done = Just Done
  thick (Keep n'm') (Keep l'n') = Keep <$> thick n'm' l'n'
  thick (Keep n'm') (Drop ln') = Drop <$> thick n'm' ln'
  thick (Drop _nm') (Keep _l'n') = Nothing
  thick (Drop nm') (Drop ln') = thick nm' ln'
