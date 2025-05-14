{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.DeBruijn.Index.Inductive (
  -- * DeBruijn indices
  Ix (FZ, FS),
  eqIx,
  toInductive,
  fromInductive,
  fromIx,
  fromIxRaw,
  isPos,
  thin,
  thick,
  inject,

  -- * Existential Wrapper
  SomeIx (..),
  withSomeIx,
  toSomeIx,
  toSomeIxRaw,
  fromSomeIx,
  fromSomeIxRaw,
) where

import Control.DeepSeq (NFData (..))
import Data.DeBruijn.Index qualified as Efficient
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Nat (type Nat (..), type Pos, type (+))
import Data.Type.Nat.Singleton.Inductive (SNat (..), SomeSNat (..), decSNat, fromSNat, fromSNatRaw, plusCommS, toSomeSNat)
import Text.Printf (printf)

{- $setup
>>> import Data.DeBruijn.Index.Inductive.Arbitrary
-}

--------------------------------------------------------------------------------
-- DeBruijn Indexes
--------------------------------------------------------------------------------

-- | @'Ix' n@ is the type of DeBruijn indices less than @n@.
type Ix :: Nat -> Type
data Ix n where
  FZ :: Ix (S n)
  FS :: Ix n -> Ix (S n)

eqIx :: Ix n -> Ix m -> Bool
eqIx FZ FZ = True
eqIx (FS i) (FS j) = eqIx i j
eqIx _ _ = False

instance NFData (Ix n) where
  rnf :: Ix n -> ()
  rnf FZ = ()
  rnf (FS i) = rnf i

instance Eq (Ix n) where
  (==) :: Ix n -> Ix n -> Bool
  (==) = eqIx

deriving instance Show (Ix n)

-- | Convert from the efficient representation 'Efficient.Ix' to the inductive representation 'Ix'.
toInductive :: Efficient.Ix n -> Ix n
toInductive Efficient.FZ = FZ
toInductive (Efficient.FS i) = FS (toInductive i)

-- | Convert from the inductive representation 'Ix' to the efficient representation 'Efficient.Ix'.
fromInductive :: Ix n -> Efficient.Ix n
fromInductive FZ = Efficient.FZ
fromInductive (FS i) = Efficient.FS (fromInductive i)

-- | Convert an 'Ix' to 'Word'.
{-# SPECIALIZE fromIx :: Ix n -> Int #-}
fromIx :: (Integral i) => Ix n -> i
fromIx = \case
  FZ -> 0
  FS i -> succ (fromIx i)

fromIxRaw :: Ix n -> Int
fromIxRaw = fromIx
{-# INLINE fromIxRaw #-}

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
inject :: SNat n -> Ix m -> Ix (n + m)
inject Z i = i
inject (S _) FZ = FZ
inject n (FS i) =
  case plusCommS n (erase i) of
    Refl -> FS (inject n i)

{-| Raise.
NOTE: Requires @'Ix' n -> 'SNat' n@, which is unprovable,
      since 'Ix' does not contain sufficient information to
      reconstruct the upper bound.
raise :: Ix n -> SNat m -> Ix (n + m)
raise i Z = _
raise i (S n) = _
-}

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

-- | An existential wrapper around indexes.
type SomeIx :: Type
data SomeIx = forall (n :: Nat). SomeIx
  { bound :: !(SNat n)
  , index :: !(Ix n)
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
toSomeIx (bound, index)
  | index < 0 = error $ printf "index cannot contain negative value, found index %d" (toInteger index)
  | bound <= index = error "bound must be larger than index, found bound %d and index %d" (toInteger bound) (toInteger index)
  | bound >= 1, index == 0, SomeSNat n <- toSomeSNat (pred bound) = SomeIx (S n) FZ
  | SomeIx n i <- toSomeIx (pred bound, pred index) = SomeIx (S n) (FS i)

{-| @'toSomeIxRaw' n@ constructs the index @n@ at type @'Ix' n@ from the 'Int' @n@.

prop> toSomeIxRaw (fromSomeIxRaw i) == i
-}
toSomeIxRaw :: (Int, Int) -> SomeIx
toSomeIxRaw = toSomeIx

-- | @'fromSomeSNat' n@ returns the numeric representation of the wrapped index.
fromSomeIx :: (Integral i) => SomeIx -> (i, i)
fromSomeIx = withSomeIx (\n i -> (fromSNat n, fromIx i))

-- | @'fromSomeSNat' n@ returns the 'Int' representation of the wrapped index.
fromSomeIxRaw :: SomeIx -> (Int, Int)
fromSomeIxRaw = withSomeIx (\n i -> (fromSNatRaw n, fromIxRaw i))

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | @`erase` x@ erases the content of @x@ to a @`Proxy`@.
erase :: f a -> Proxy a
erase _ = Proxy
{-# INLINE erase #-}
