open import Agda.Primitive

module Indexed {i : Level} {I : Set i} where

open import Agda.Builtin.Sigma

ISet : (ℓ : Level) -> Set (i ⊔ lsuc ℓ)
ISet ℓ = @0 I -> Set ℓ

All : ∀ {p} (P : ISet p) -> Set _
All P = {@0 x : I} -> P x

infixr 4 _:->_

_:->_ : ∀ {p q} (P : ISet p) (Q : ISet q) -> ISet (p ⊔ q)
_:->_ P Q t = P t -> Q t


-- _:->_ : {ℓ : Level} {I : Set ℓ} (S T : @0 I -> Set ℓ) (@0 i : I) -> Set ℓ
-- _:->_ S T i = S i -> T i

-- _:*_ : {ℓ : Level} {I : Set ℓ} (S T : @0 I -> Set ℓ) (@0 i : I) -> Set ℓ
-- _:*_ S T i = Σ (S i) (λ _ -> T i)

-- K : {ℓ : Level} {I : Set ℓ} -> Set ℓ -> @0 I -> Set ℓ
-- K A i = A

-- record All {ℓ : Level} {I : Set ℓ} (S : @0 I -> Set ℓ) : Set ℓ where
--   field
--     value : {@0 i : I} -> S i

-- record Any {ℓ : Level} {I : Set ℓ} (S : @0 I -> Set ℓ) : Set ℓ where
--   field
--     @0 {index} : I
--     value : S index
