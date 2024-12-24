module GilearW where

variable
  A : Set
  B : Set

record _*_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B

module Indexed (I : Set) where

  variable
    i : I
    S : @0 I -> Set
    T : @0 I -> Set

  _=>_ : (S T : @0 I -> Set) (@0 i : I) -> Set
  _=>_ S T i = S i -> T i


  _**_ : (S T : @0 I -> Set) (@0 i : I) -> Set
  _**_ S T i = S i * T i

  K : Set -> @0 I -> Set
  K A i = A

  All : (S : @0 I -> Set) -> Set
  All S = {@0 i : I} -> S i

  data Any (S : @0 I -> Set) : Set where
    any : { @0 i : I } -> S i -> Any S

  liftK : (A -> B) -> All (K A => K B)
  liftK f {i} a = f a

module Timed (Time : Set) (tick : Time -> Time) where
