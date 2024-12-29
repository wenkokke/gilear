module GilearW.Internal.Ix where

open import Haskell.Prelude renaming (zero to Z; suc to S)

{-# FOREIGN AGDA2HS
-- | The type of DeBruijn indices.
newtype Ix = UnsafeIx Word
  deriving (Eq, Ord)
#-}

{-# FOREIGN AGDA2HS
-- | Take the precessor of the DeBruijn index.
safePred :: Ix -> Maybe Ix
safePred (UnsafeIx n)
  | n == 0 = Nothing
  | otherwise = Just $ UnsafeIx (n - 1)
#-}

{-# FOREIGN AGDA2HS
-- | Zero.
pattern FZ :: Ix
pattern FZ <- (safePred -> Nothing)
  where
    FZ = UnsafeIx 0
#-}

{-# FOREIGN AGDA2HS
-- | Successor.
pattern FS :: Ix -> Ix
pattern FS n <- (safePred -> Just n)
  where
    FS (UnsafeIx n) = UnsafeIx (1 + n)
#-}

{-# FOREIGN AGDA2HS
{-# COMPLETE FZ, FS #-}
#-}

data Ix : (@0 n : Nat) -> Set where
  FZ : {@0 n : Nat} -> Ix (S n)
  FS : {@0 n : Nat} -> Ix n -> Ix (S n)

thin : {@0 n : Nat} -> Ix (S n) -> Ix n -> Ix (S n)
thin  FZ        y  = FS y
thin (FS _)  FZ    = FZ
thin (FS x) (FS y) = FS (thin x y)
{-# COMPILE AGDA2HS thin #-}

data Pos : (@0 n : Nat) -> Set where
  MkPos : {@0 n : Nat} -> Pos (S n)
{-# COMPILE AGDA2HS Pos #-}

isPos : {@0 n : Nat} -> Ix n -> Pos n
isPos  FZ    = MkPos
isPos (FS _) = MkPos
{-# COMPILE AGDA2HS isPos #-}

thick : {@0 n : Nat} -> Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick  FZ     FZ    = Nothing
thick  FZ    (FS j) = Just j
thick (FS i)  FZ    = case isPos i of \{ MkPos -> Just FZ }
thick (FS i) (FS j) = case isPos i of \{ MkPos -> FS <$> thick i j}
{-# COMPILE AGDA2HS thick #-}
