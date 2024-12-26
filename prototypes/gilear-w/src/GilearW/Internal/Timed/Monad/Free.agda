module GilearW.Internal.Timed.Monad.Free where

{-# FOREIGN AGDA2HS
import Data.Kind (Type)
import GilearW.Internal.Timed.Monad (TimedMonadReturn (..), TimedMonad (..))
#-}
open import Level using (Level; _⊔_)
open import Haskell.Prelude hiding (All; _∘_)
open import GilearW.Internal.Common
open import GilearW.Internal.Indexed
open import GilearW.Internal.Time
open import GilearW.Internal.Timed
open import GilearW.Internal.Timed.Monad

{-# FOREIGN AGDA2HS
data Free (p :: Type) (c :: Type -> Type) (v :: Type) :: Type where
  Pure :: v -> Free p c v
  Call ::
    forall (p :: Type) (c :: Type -> Type) (v :: Type) (w :: Type).
    c w ->
    (p -> w -> Free p c v) ->
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
    ({@0 t : Time} -> p s t -> w t -> Free p c v t) ->
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
  {{superp : TimedMonoid p}} ->
  {c : (@0 Time -> Set) -> @0 Time -> Set}
  {v : @0 Time -> Set} ->
  {{superv : Timed p v}} ->
  {w : @0 Time -> Set} ->
  {{superw : Timed p w}} ->
  {@0 s : Time} ->
  Free p c v s ->
  ({@0 t : Time} -> p s t -> Kripke p v t -> Free p c w t) ->
  Free p c w s
tbind {{superp}} (Pure v)   k =
  k (tempty {{superp}}) (kripke v)
tbind {{superp}} (Call c j) k =
  Call c λ psr wr →
    tbind {{superp}} (j psr wr) λ prt kpvt →
      k (tappend {{superp}} psr prt) kpvt
{-# COMPILE AGDA2HS tbind #-}

instance
  iTimedFree :
    {p : @0 Time -> @0 Time -> Set} ->
    {{ superp : TimedMonoid p }} ->
    {c : (@0 Time -> Set) -> @0 Time -> Set} ->
    {{ superc : Forall (Compose (Timed p) c) }} ->
    {v : @0 Time -> Set} ->
    {{ superv : Timed p v }} ->
    Timed p (Free p c v)
  iTimedFree {p} {{superp}} {c} {{superc}} {v} {{superv}} =
    record { super = superp; _&>_ = go }
    where
      go : {@0 s t : Time} → Free p c v s → p s t → Free p c v t
      go (Pure v)   pst = Pure (v &> pst)
      go (Call c k) pst = Call (c &> pst) \ prt wt ->
        k (tappend {{superp}} pst prt) wt
  {-# COMPILE AGDA2HS iTimedFree #-}

instance
  iTimedMonadReturnFree :
    {p : @0 Time -> @0 Time -> Set} ->
    {c : (@0 Time -> Set) -> @0 Time -> Set} ->
    TimedMonadReturn (Free p c)
  iTimedMonadReturnFree {p} {c} = record 
    { return = treturn
    }
  {-# COMPILE AGDA2HS iTimedMonadReturnFree #-}

instance
  iTimedMonadFree :
    {p : @0 Time -> @0 Time -> Set} ->
    {{super : TimedMonoid p}} ->
    {c : (@0 Time -> Set) -> @0 Time -> Set} ->
    TimedMonad p (Free p c)
  iTimedMonadFree {p} {{super}} {c} = record 
    { _>>=_  = tbind {p} {{super}}
    }
  {-# COMPILE AGDA2HS iTimedMonadFree #-}
