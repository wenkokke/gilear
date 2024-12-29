module GilearW.Internal.Name where

open import Agda.Builtin.String using (String)
open import Haskell.Prelude using (Word)

record Name : Set where
  field
    name : String
    uniq : Word
open Name public
{-# COMPILE AGDA2HS Name #-}
 