module GilearW.Internal.Timed.Monad where

open import Level using (Level; _⊔_) renaming (suc to lsuc)
open import GilearW.Internal.Indexed
open import GilearW.Internal.Timed
open import GilearW.Internal.Time
open import Haskell.Prelude hiding (All)

record TimedMonadReturn
  {li : Level}
  {lo : Level}
  (m : (@0 Time -> Set li) -> @0 Time -> Set lo) : 
  Set (lsuc li ⊔ lo) where
  field
    return :
      {v : @0 Time -> Set li} ->
      All (v -:> m v)
open TimedMonadReturn public
{-# COMPILE AGDA2HS TimedMonadReturn class #-}


record TimedMonad
  {lp : Level}
  (p : @0 Time -> @0 Time -> Set lp)
  {li : Level}
  {lo : Level}
  (m : (@0 Time -> Set li) -> @0 Time -> Set lo) : 
  Set (lp ⊔ lsuc li ⊔ lo) where
  field
    overlap {{ iTimedMonoid }} : TimedMonoid p
    overlap {{ iTimedMonadReturn }} : TimedMonadReturn m
    _>>=_ :
      {v : @0 Time -> Set li} ->
      {{iTimedV : Timed p v}} ->
      {w : @0 Time -> Set li} ->
      {{iTimedW : Timed p w}} ->
      ∀ {@0 s} ->
      m v s ->
      (∀ {@0 t} -> p s t -> Kripke p v t -> m w t) ->
      m w s
open TimedMonad public
{-# COMPILE AGDA2HS TimedMonad class #-}
