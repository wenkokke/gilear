{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.DeBruijn (
  Ix (FZ, FS),
  toNatural,
  safePred,
  raiseBy,
  raise,
  injectBy,
  inject,
  tabulate,
  SomeIx (..),

  -- * Re-export 'GHC.TypeNats'
  Natural,
  Z,
  S,
  KnownNat (natSing),
  SNat,
  fromSNat,
  type (+),
) where

import GHC.Num.Natural (Natural, naturalIsZero, naturalSubUnsafe, naturalZero)
import GHC.TypeNats (KnownNat (..), SNat, fromSNat, type (+))

-- | Zero.
type Z :: Natural
type Z = 0

-- | Successor.
type S :: Natural -> Natural
type S n = 1 + n

-- | The type of DeBruijn indices.
newtype Ix (n :: Natural) = UnsafeIx {unIx :: Natural}
  deriving (Eq, Ord)

-- | Convert a DeBruijn index 'Ix' to the underlying 'Natural'.
toNatural :: Ix n -> Natural
toNatural = unIx

-- | Zero.
pattern FZ :: Ix (S n)
pattern FZ <- (safePred -> Nothing)
  where
    FZ = UnsafeIx naturalZero

-- | Successor.
pattern FS :: Ix n -> Ix (S n)
pattern FS n <- (safePred -> Just n)
  where
    FS (UnsafeIx n) = UnsafeIx (1 + n)

{-# COMPLETE FZ, FS #-}

-- | Take the precessor of the DeBruijn index.
safePred :: Ix (S n) -> Maybe (Ix n)
safePred (UnsafeIx n)
  | naturalIsZero n = Nothing
  | otherwise = Just (UnsafeIx (n `naturalSubUnsafe` 1))

-- | Raise the value of a DeBruijn index by some known natural `m`.
raiseBy :: SNat m -> Ix n -> Ix (m + n)
raiseBy m (UnsafeIx n) = UnsafeIx (fromSNat m + n)

-- | Raise the value of a DeBruijn index by some known natural `m`.
raise :: (KnownNat m) => Ix n -> Ix (m + n)
raise = raiseBy natSing

-- | Raise the range of a DeBruijn index by some known natural `m`.
injectBy :: SNat m -> Ix n -> Ix (m + n)
injectBy _m (UnsafeIx index) = UnsafeIx index

-- | Raise the range of a DeBruijn index by some known natural `m`.
inject :: (KnownNat m) => Ix n -> Ix (m + n)
inject = injectBy natSing

-- | List all DeBruijn indices between `0` and some known natural `n`.
tabulate :: SNat n -> [Ix n]
tabulate n = fmap UnsafeIx [0 .. fromSNat n]

instance Show (Ix n) where
  show :: Ix n -> String
  show (UnsafeIx n) = show n

-- | An existential wrapper for DeBruijn indices.
data SomeIx = forall n. SomeIx
  { ixBound :: SNat n
  , ixValue :: Ix n
  }

instance Show SomeIx where
  show :: SomeIx -> String
  show (SomeIx _bound value) = show value
