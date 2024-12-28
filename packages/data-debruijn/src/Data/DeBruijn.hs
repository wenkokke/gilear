{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DeBruijn where

import Data.Kind (Type)

-- | The type of natural numbers.
data N
  = Z
  | S N

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
