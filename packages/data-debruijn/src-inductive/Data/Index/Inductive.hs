{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Data.Index.Inductive (
  -- * DeBruijn indices
  Ix (FZ, FS),
  toInductive,
  fromInductive,
  fromIx,
  fromIxRaw,
  isPos,
  thin,
  thick,
  inject,
) where

import Data.Index qualified as Efficient
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import Data.Type.Nat (type Nat (..), type Pos, type (+))
import Data.Type.Nat.Singleton.Inductive (SNat (..), plusCommS)

-- | @'Ix' n@ is the type of DeBruijn indices less than @n@.
type Ix :: Nat -> Type
data Ix n where
  FZ :: Ix (S n)
  FS :: !(Ix n) -> Ix (S n)

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
  case plusCommS n (eraseIx i) of
    Refl -> FS (inject n i)

{-| Raise.
NOTE: Requires @'Ix' n -> 'SNat' n@, which is unprovable,
      since 'Ix' does not contain sufficient information to
      reconstruct the upper bound.
raise :: Ix n -> SNat m -> Ix (n + m)
raise i Z = _
raise i (S n) = _
-}
eraseIx :: Ix n -> Proxy n
eraseIx _ = Proxy
