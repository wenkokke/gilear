{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DeBruijn (
  N (Z, S),
  Ix (FZ, FS),
  Pos (Pos),
  isPos,
  thin,
  thick,
  toWord,
) where

import Data.Kind (Type)

-- | The type of natural numbers.
data N
  = Z
  | S N

-- | Type-level addition of natural numbers.
type family (+) (n :: N) (m :: N) :: N where
  Z + m = m
  S n + m = S (n + m)

-- | The type of de Bruijn indices.
data Ix (n :: N) :: Type where
  FZ :: Ix (S n)
  FS :: Ix n -> Ix (S n)

deriving stock instance (Eq (Ix n))

deriving stock instance (Ord (Ix n))

-- | The type of evidence that @n@ is positive.
data Pos (n :: N) :: Type where
  Pos :: Pos (S n)

deriving stock instance (Eq (Pos n))

deriving stock instance (Ord (Pos n))

-- | The bound of any de Bruijn index is positive.
isPos :: Ix n -> Pos n
isPos FZ = Pos
isPos (FS _) = Pos

-- | Convert the de Bruijn index to a 'Word'.
toWord :: Ix n -> Word
toWord FZ = 0
toWord (FS i) = 1 + toWord i

-- | Thinning.
thin :: Ix (S n) -> Ix n -> Ix (S n)
thin FZ j = FS j
thin (FS _) FZ = FZ
thin (FS i) (FS j) = FS (thin i j)

-- | Thickening.
thick :: Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick FZ FZ = Nothing
thick FZ (FS j) = Just j
thick (FS i) FZ = case isPos i of Pos -> Just FZ
thick (FS i) (FS j) = case isPos i of Pos -> FS <$> thick i j

-- unsafeLowerPos :: Pos (S n) -> Pos n
-- unsafeLowerPos = unsafeCoerce

-- -- | The type of DeBruijn indices.
-- data Ix (n :: Natural) :: Type where
--   UnsafeIx :: Pos n -> Natural -> Ix n

-- deriving stock instance (Eq (Ix n))

-- deriving stock instance (Ord (Ix n))

-- -- | Convert a DeBruijn index 'Ix' to the underlying 'Natural'.
-- toNatural :: Ix n -> Natural
-- toNatural (UnsafeIx _ n) = n

-- -- | Zero.
-- pattern FZ :: Ix (S n)
-- pattern FZ <- (safePred -> Nothing)
--   where
--     FZ = UnsafeIx Pos naturalZero

-- -- | Successor.
-- pattern FS :: Ix n -> Ix (S n)
-- pattern FS n <- (safePred -> Just n)
--   where
--     FS (UnsafeIx Pos n) = UnsafeIx Pos (1 + n)

-- {-# COMPLETE FZ, FS #-}

-- -- | Take the precessor of the DeBruijn index.
-- safePred :: Ix (S n) -> Maybe (Ix n)
-- safePred (UnsafeIx p n)
--   | naturalIsZero n = Nothing
--   | otherwise = Just (UnsafeIx (unsafeLowerPos p) (n `naturalSubUnsafe` 1))

-- -- | Thinning.
-- thin :: Ix (S n) -> Ix n -> Ix (S n)
-- thin  FZ        i  = FS i
-- thin (FS j)  FZ    = FZ
-- thin (FS j) (FS i) = FS (thin j i)

-- -- -- | Raise the value of a DeBruijn index by some known natural `m`.
-- -- raiseBy :: SNat m -> Ix n -> Ix (m + n)
-- -- raiseBy m (UnsafeIx n) = UnsafeIx (fromSNat m + n)

-- -- -- | Raise the value of a DeBruijn index by some known natural `m`.
-- -- raise :: (KnownNat m) => Ix n -> Ix (m + n)
-- -- raise = raiseBy natSing

-- -- -- | Raise the range of a DeBruijn index by some known natural `m`.
-- -- injectBy :: SNat m -> Ix n -> Ix (m + n)
-- -- injectBy _m (UnsafeIx index) = UnsafeIx index

-- -- -- | Raise the range of a DeBruijn index by some known natural `m`.
-- -- inject :: (KnownNat m) => Ix n -> Ix (m + n)
-- -- inject = injectBy natSing

-- -- -- | List all DeBruijn indices between `0` and some known natural `n`.
-- -- tabulate :: SNat n -> [Ix n]
-- -- tabulate n = fmap UnsafeIx [0 .. fromSNat n]

-- -- instance Show (Ix n) where
-- --   show :: Ix n -> String
-- --   show (UnsafeIx n) = show n

-- -- -- | An existential wrapper for DeBruijn indices.
-- -- data SomeIx = forall n. SomeIx
-- --   { ixBound :: SNat n
-- --   , ixValue :: Ix n
-- --   }

-- -- instance Show SomeIx where
-- --   show :: SomeIx -> String
-- --   show (SomeIx _bound value) = show value
