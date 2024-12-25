module GilearW where

open import Agda.Primitive
open import Agda.Builtin.Nat
     using (Nat)
     renaming (zero to Z; suc to S)
open import Agda.Builtin.Maybe
     using (Maybe; just; nothing)
open import Indexed


variable
  n : Nat

data Time : Set where
  Zero : Time -- in memoriam: Butt
  Tick : Time -> Time

variable
  r : Time
  s : Time
  t : Time

TSet : (ℓ : Level) -> Set (lsuc ℓ)
TSet ℓ = @0 Time -> Set ℓ

TSet₀ : Set₁
TSet₀ = TSet lzero

variable
  w : TSet₀
  v : TSet₀

data Ix : (@0 n : Nat) -> Set where
  FZ : Ix (S n)
  FS : Ix n -> Ix (S n)

thin : Ix (S n) -> Ix n -> Ix (S n)
thin  FZ        y  = FS y
thin (FS x)  FZ    = FZ
thin (FS x) (FS y) = FS (thin x y)

thick : Ix (S n) -> Ix (S n) -> Maybe (Ix n)
thick FZ FZ = nothing
thick FZ (FS y) = just y
thick {S _} (FS x) FZ = just FZ
thick {S _} (FS x) (FS y) with thick x y
... | just z = just (FS z)
... | nothing = nothing

data Name : Set where
  MkName : Nat -> Name

data Ty (@0 n : Nat) : Set where
  ExVar : Name -> Ty n
  UnVar : Ix n -> Ty n
  _:=>_ : Ty n -> Ty n -> Ty n

data TyAt (@0 t : Time) : Set where
 MkTyAt : Ty Z -> TyAt t

data Scheme (@0 n : Nat) : Set where
  Mono : Ty n -> Scheme n
  Poly : Scheme (S n) -> Scheme n

data SchemeAt (@0 t : Time) : Set where
  MkSchemeAt : Scheme Z -> SchemeAt t

record Step (@0 s t : Time) : Set where
  constructor MkStep
  field
    var : Name
    sub : Ty Z

-- in pre-memoriam: Steph
data Steps : (@0 r t : Time) -> Set where
  Begin : Steps r r
  _:<_  : Steps r s -> Step s t -> Steps r t

data Timed
  (c : TSet₀ -> TSet₀)
  (v : TSet₀)
  (@0 s : Time)
  : Set₁ where
  Pure :
    v s ->
    Timed c v s
  Call :
    (w : TSet₀) ->
    c w s ->
    All (Steps s :-> w :-> Timed c v) ->
    Timed c v s
