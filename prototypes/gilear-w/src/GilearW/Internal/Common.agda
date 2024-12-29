module GilearW.Internal.Common where

{-# FOREIGN AGDA2HS
import Data.Kind (Constraint, Type)
#-}
open import Level using (Level; _⊔_)

-- Only sensible things here

Forall :
  {la : Level} ->
  {lp : Level} ->
  {a : Set la} ->
  (p : a -> Set lp) ->
  Set (la ⊔ lp)
Forall p = ∀ {x} -> p x

{-# FOREIGN AGDA2HS
type Forall a p = forall (x :: a). p x
#-}

Compose : 
  {la : Level}
  {lb : Level}
  {lc : Level}
  {a : Set la}
  {b : Set lb}
  {c : Set lc} ->
  (b -> c) ->
  (a -> b) ->
  (a -> c)
Compose f g = \ x -> f (g x)

{-# FOREIGN AGDA2HS
-- TODO: a, b and c are kind; can we simplify this?
type Compose ka kb kc p q x = p (q x)
#-}
