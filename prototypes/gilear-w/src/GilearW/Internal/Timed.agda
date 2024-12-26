module GilearW.Internal.Timed where

open import Level using (Level; _⊔_)
open import GilearW.Internal.Indexed
open import GilearW.Internal.Time
open import Haskell.Prelude hiding (All)

record TimedMonoid
  {lp : Level}
  (p : @0 Time -> @0 Time -> Set lp) :
  Set lp where
  field
    tempty :
      {@0 r : Time} ->
      p r r
    tappend :
      {@0 r s t : Time} ->
      p r s ->
      p s t ->
      p r t 
open TimedMonoid {{...}} public
{-# COMPILE AGDA2HS TimedMonoid class #-}

record Timed
  {lp : Level}
  (p : @0 Time -> @0 Time -> Set lp)
  {lv : Level}
  (v : @0 Time -> Set lv) :
    Set (lp ⊔ lv) where
  field
    overlap {{ super }} : TimedMonoid p 
    _&>_ : ∀ {@0 s t} -> v s -> p s t -> v t
open Timed {{...}} public
{-# COMPILE AGDA2HS Timed class #-}

Kripke :
  {lp : Level}
  (p : @0 Time -> @0 Time -> Set lp)
  {lv : Level}
  (v : @0 Time -> Set lv)
  (@0 s : Time) ->
  Set _
Kripke p v s = {@0 t : Time} -> p s t -> v t
{-# COMPILE AGDA2HS Kripke #-}

kripke :
  {lp : Level}
  {p : @0 Time -> @0 Time -> Set lp}
  {lv : Level}
  {v : @0 Time -> Set lv}
  {{ iTimedPV : Timed p v }} ->
  All (v -:> Kripke p v)
kripke v pst = v &> pst
{-# COMPILE AGDA2HS kripke #-}
