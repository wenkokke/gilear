{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module GilearW.Internal.Ix where

-- | The type of DeBruijn indices.
newtype Ix = UnsafeIx Word
  deriving (Eq, Ord)

-- | Take the precessor of the DeBruijn index.
safePred :: Ix -> Maybe Ix
safePred (UnsafeIx n)
  | n == 0 = Nothing
  | otherwise = Just $ UnsafeIx (n - 1)

-- | Zero.
pattern FZ :: Ix
pattern FZ <- (safePred -> Nothing)
  where
    FZ = UnsafeIx 0

-- | Successor.
pattern FS :: Ix -> Ix
pattern FS n <- (safePred -> Just n)
  where
    FS (UnsafeIx n) = UnsafeIx (1 + n)
