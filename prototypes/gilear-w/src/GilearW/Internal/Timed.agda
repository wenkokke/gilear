module GilearW.Internal.Timed where

open import Level using (Level; _⊔_)
open import GilearW.Internal.Indexed
open import GilearW.Internal.Time
open import Haskell.Prelude hiding (All; seq)

data Star
  {lp : Level}
  (p : @0 Time -> @0 Time -> Set lp)
  (@0 r : Time) :
  (@0 t : Time) -> Set lp where
  Now : Star p r r 
  Seq : {@0 s t : Time} -> Star p r s -> p s t -> Star p r t
{-# COMPILE AGDA2HS Star #-}

seq :
  {lp : Level}
  {p : @0 Time -> @0 Time -> Set lp} ->
  {@0 r s t : Time} ->
  Star p r s ->
  Star p s t ->
  Star p r t
seq prs  Now          = prs
seq prs (Seq pst ptu) = Seq (seq prs pst) ptu
{-# COMPILE AGDA2HS seq #-}

record Timed
  {lp : Level}
  (p : @0 Time -> @0 Time -> Set lp)
  {lv : Level}
  (v : @0 Time -> Set lv) :
    Set (lp ⊔ lv) where
  field
    update : ∀ {@0 s t} -> Star p s t -> v s -> v t
open Timed {{...}} public
{-# COMPILE AGDA2HS Timed class #-}

Kripke :
  {lp : Level}
  (p : @0 Time -> @0 Time -> Set lp)
  {lv : Level}
  (v : @0 Time -> Set lv)
  (@0 s : Time) ->
  Set _
Kripke p v s = {@0 t : Time} -> Star p s t -> v t
{-# COMPILE AGDA2HS Kripke #-}

kripke :
  {lp : Level}
  {p : @0 Time -> @0 Time -> Set lp}
  {lv : Level}
  {v : @0 Time -> Set lv}
  {{ iTimedV : Timed p v }} ->
  All (v -:> Kripke p v)
kripke v pst = update pst v
{-# COMPILE AGDA2HS kripke #-}
