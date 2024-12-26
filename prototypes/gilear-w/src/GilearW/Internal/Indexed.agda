open import Level using (Level; _⊔_) renaming (zero to lzero; suc to lsuc)

module GilearW.Internal.Indexed where

open import Data.Product using (_×_) renaming (proj₁ to fst; proj₂ to snd)
-- open import Haskell.Prelude using () renaming (_×_ to _*_)

All :
    {li : Level} ->
    {@0 i : Set li} ->
    {lp : Level} ->
    (p : @0 i -> Set lp) ->
    Set _
All {li} {i} {lp} p = {@0 x : i} -> p x
{-# COMPILE AGDA2HS All #-}

infixr 0 _:->_

_:->_ :
    {li : Level} ->
    {@0 i : Set li} ->
    {lp : Level} ->
    (p : @0 i -> Set lp) ->
    {lq : Level} ->
    (q : @0 i -> Set lq) ->
    (@0 x : i) ->
    Set (lp ⊔ lq)
_:->_ p q x = p x -> q x
{-# COMPILE AGDA2HS _:->_ #-}

record _:*_
    {li : Level}
    {@0 i : Set li}
    {lp : Level}
    (p : @0 i -> Set lp)
    {lq : Level}
    (q : @0 i -> Set lq)
    (@0 x : i) :
    Set (lp ⊔ lq)
  where
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
    Set lp
    where
    field
        unK : p
{-# COMPILE AGDA2HS K newtype #-}

open K public

-- record All {l : Level} {I : Set l} (S : @0 I -> Set l) : Set l where
--   field
--     value : {@0 i : I} -> S i

-- record Any {l : Level} {I : Set l} (S : @0 I -> Set l) : Set l where
--   field
--     @0 {index} : I
--     value : S index
  