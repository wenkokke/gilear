module GilearW where

open import Level using (Level) renaming (zero to lzero; suc to lsuc)
open import GilearW.Internal.Indexed
open import Haskell.Prelude renaming (zero to Z; suc to S)
{-# FOREIGN AGDA2HS import GilearW.Internal.Ix (Ix (..)) #-}

data Time : Set where
  Tick : Time -> Time

data Ix : (@0 n : Nat) -> Set where
  FZ : {@0 n : Nat} -> Ix (S n)
  FS : {@0 n : Nat} -> Ix n -> Ix (S n)

thin : {@0 n : Nat} -> Ix (S n) -> Ix n -> Ix (S n)
thin  FZ        y  = FS y
thin (FS _)  FZ    = FZ
thin (FS x) (FS y) = FS (thin x y)
{-# COMPILE AGDA2HS thin #-}

-- data Pos : (@0 n : Nat) -> Set where
--   MkPos : {@0 n : Nat} -> Pos (S n)
-- {-# COMPILE AGDA2HS Pos #-}

-- isPos : {@0 n : Nat} -> Ix n -> Pos n
-- isPos  FZ    = MkPos
-- isPos (FS i) = MkPos
-- {-# COMPILE AGDA2HS isPos #-}

-- thick : {@0 n : Nat} -> Ix (S n) -> Ix (S n) -> Maybe (Ix n)
-- thick  FZ     FZ    = Nothing
-- thick  FZ    (FS j) = Just j
-- thick (FS i)  FZ    = case isPos i of \{ MkPos -> Just FZ }
-- thick (FS i) (FS j) = case isPos i of \{ MkPos -> FS <$> thick i j}
-- {-# COMPILE AGDA2HS thick #-}

-- data Name : Set where
--   MkName : Word -> Name
-- {-# COMPILE AGDA2HS Name #-}

-- data Ty (@0 n : Nat) : Set where
--   E : Name -> Ty n
--   U : Ix n -> Ty n
--   _:=>_ : Ty n -> Ty n -> Ty n
-- {-# COMPILE AGDA2HS Ty #-}

-- -- data TyAt (@0 t : Time) : Set where
-- --  MkTyAt : Ty Z -> TyAt t

-- -- data Scheme (@0 n : Nat) : Set where
-- --   Mono : Ty n -> Scheme n
-- --   Poly : Scheme (S n) -> Scheme n

-- -- data SchemeAt (@0 t : Time) : Set where
-- --   MkSchemeAt : Scheme Z -> SchemeAt t

-- -- record Step (@0 s t : Time) : Set where
-- --   constructor MkStep
-- --   field
-- --     var : Name
-- --     sub : Ty Z

-- -- -- in pre-memoriam: Steph
-- -- data Steps : (@0 r t : Time) -> Set where
-- --   Begin : Steps r r
-- --   _:<_  : Steps r s -> Step s t -> Steps r t

-- -- data Timed
-- --   (c : TSet₀ -> TSet₀)
-- --   (v : TSet₀)
-- --   (@0 s : Time)
-- --   : Set₁ where
-- --   Pure :
-- --     v s ->
-- --     Timed c v s
-- --   Call :
-- --     (w : TSet₀) ->
-- --     c w s ->
-- --     All (Steps s :-> w :-> Timed c v) ->
-- --     Timed c v s
