module GilearW where

{-# FOREIGN AGDA2HS
import GilearW.Internal.Ix
import GilearW.Internal.Name
import GilearW.Internal.Time
#-}
open import Level using (Level)
open import GilearW.Internal.Indexed
open import GilearW.Internal.Ix
open import GilearW.Internal.Name
open import GilearW.Internal.Time
open import Haskell.Prelude
  renaming (zero to Z; suc to S)
  hiding (All)

  
data Ty (@0 n : Nat) : Set where
  E : Name -> Ty n
  U : Ix n -> Ty n
  _:=>_ : Ty n -> Ty n -> Ty n
{-# COMPILE AGDA2HS Ty #-}

record TyAt (@0 t : Time) : Set where
  inductive
  field
    unTyAt : Ty Z -> TyAt t
open TyAt public
{-# COMPILE AGDA2HS TyAt newtype #-}

data Sch (@0 n : Nat) : Set where
  T : Ty n         -> Sch n
  P : Sch (S n) -> Sch n
{-# COMPILE AGDA2HS Sch #-}

record SchAt (@0 t : Time) : Set where
  inductive
  field
    unSchAt : Sch Z -> SchAt t
open SchAt public
{-# COMPILE AGDA2HS SchAt newtype #-}

-- Step
record Sub (@0 s t : Time) : Set where
  field
    targetName : Name
    replacementTy : Ty Z
open Sub public
{-# COMPILE AGDA2HS Sub #-}

-- &>
data Subs (@0 r : Time) : (@0 t : Time) -> Set where
  Id   : Subs r r 
  _:<_ : {@0 s t : Time} -> Subs r s -> Sub s t -> Subs r t
{-# COMPILE AGDA2HS Subs #-}



