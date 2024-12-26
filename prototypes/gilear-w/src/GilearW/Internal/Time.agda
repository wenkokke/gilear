module GilearW.Internal.Time where

open import Haskell.Prelude

data Time : Set where
  Tick : Time -> Time
