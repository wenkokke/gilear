module GilearW.Internal.Indexed where

open import Level using (Level; _⊔_) renaming (zero to lzero; suc to lsuc)
open import Data.Product using (_×_) renaming (proj₁ to fst; proj₂ to snd)

infixr 0 _-:>_

_-:>_ :
    {li : Level} ->
    {@0 i : Set li} ->
    {lp : Level} ->
    (p : @0 i -> Set lp) ->
    {lq : Level} ->
    (q : @0 i -> Set lq) ->
    (@0 x : i) ->
    Set (lp ⊔ lq)
_-:>_ p q x = p x -> q x
{-# COMPILE AGDA2HS _-:>_ #-}

record _:*_
    {li : Level}
    {@0 i : Set li}
    {lp : Level}
    (p : @0 i -> Set lp)
    {lq : Level}
    (q : @0 i -> Set lq)
    (@0 x : i) :
        Set (lp ⊔ lq) where
  constructor _,_
  field
    fst : p x
    snd : q x
open _:*_ public
{-# COMPILE AGDA2HS _:*_ tuple #-}

record K
    {li : Level}
    {@0 i : Set li}
    {lp : Level}
    (p : Set lp)
    (@0 x : i) :
        Set lp where
    field
        unK : p
open K public
{-# COMPILE AGDA2HS K newtype #-}

All :
    {li : Level} ->
    {@0 i : Set li} ->
    {lp : Level} ->
    (p : @0 i -> Set lp) ->
    Set _
All {li} {i} {lp} p = {@0 x : i} -> p x
{-# COMPILE AGDA2HS All #-}

record Any
    {li : Level}
    {@0 i : Set li}
    {lp : Level}
    (p : @0 i -> Set lp) :
        Set (li ⊔ lp) where
  field
    @0 {x} : i
    unAny : p x
open Any public
{-# COMPILE AGDA2HS Any newtype #-}
