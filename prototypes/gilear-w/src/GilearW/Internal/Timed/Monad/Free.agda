module GilearW.Internal.Timed.Monad.Free where

{-# FOREIGN AGDA2HS
import Data.Kind (Type)
import GilearW.Internal.Timed.Monad (TimedMonad (..))
#-}
open import Level using (Level; _⊔_)
open import Haskell.Prelude hiding (All; seq)
open import GilearW.Internal.Common
open import GilearW.Internal.Indexed
open import GilearW.Internal.Time
open import GilearW.Internal.Timed
open import GilearW.Internal.Timed.Monad

{-# FOREIGN AGDA2HS
data Free (p :: Type) (c :: Type -> Type) (v :: Type) :: Type where
  Pure ::
    v ->
    Free p c v
  Call ::
    forall (p :: Type) (c :: Type -> Type) (v :: Type) (w :: Type).
    c w ->
    (Star p -> w -> Free p c v) ->
    Free p c v
#-}

data Free
  (p : @0 Time -> @0 Time -> Set)
  (c : (@0 Time -> Set) -> @0 Time -> Set)
  (v : @0 Time -> Set)
  (@0 s : Time)
  : Set₁ where
  Pure :
    v s ->
    Free p c v s
  Call : 
    {w : @0 Time -> Set} ->
    c w s ->
    ({@0 t : Time} -> Star p s t -> w t -> Free p c v t) ->
    Free p c v s

treturn :
  {p : @0 Time -> @0 Time -> Set}
  {c : (@0 Time -> Set) -> @0 Time -> Set}
  {v : @0 Time -> Set}
  {@0 s : Time} ->
  v s ->
  Free p c v s
treturn = Pure
{-# COMPILE AGDA2HS treturn #-}

tbind :
  {p : @0 Time -> @0 Time -> Set}
  {c : (@0 Time -> Set) -> @0 Time -> Set}
  {v : @0 Time -> Set} ->
  {{iTimedV : Timed p v}} ->
  {w : @0 Time -> Set} ->
  {{iTimedW : Timed p w}} ->
  {@0 s : Time} ->
  Free p c v s ->
  ({@0 t : Time} -> Star p s t -> Kripke p v t -> Free p c w t) ->
  Free p c w s
tbind (Pure v)   k =
  k Now (kripke v)
tbind (Call c j) k =
  Call c λ psr wr →
    tbind (j psr wr) λ prt kpvt →
      k (seq psr prt) kpvt
{-# COMPILE AGDA2HS tbind #-}

instance
  iTimedFree :
    {p : @0 Time -> @0 Time -> Set} ->
    {c : (@0 Time -> Set) -> @0 Time -> Set} ->
    {{iTimedC : Forall (Compose (Timed p) c)}} ->
    {v : @0 Time -> Set} ->
    {{iTimedV : Timed p v}} ->
    Timed p (Free p c v)
  iTimedFree {p} {c} {{superc}} {v} {{superv}} =
    record { update = go }
    where
      go : {@0 s t : Time} → Star p s t → Free p c v s → Free p c v t
      go pst (Pure v)   = Pure (update pst v)
      go psr (Call c k) = Call (update psr c) \ prt -> k (seq psr prt)
  {-# COMPILE AGDA2HS iTimedFree #-}

instance
  iTimedMonadFree :
    {p : @0 Time -> @0 Time -> Set} ->
    {c : (@0 Time -> Set) -> @0 Time -> Set} ->
    TimedMonad p (Free p c)
  iTimedMonadFree {p} {c} = record 
    { return = treturn
    ; _>>=_  = tbind
    }
  {-# COMPILE AGDA2HS iTimedMonadFree #-}
 