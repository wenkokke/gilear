module GilearW.Internal.Indexed where

type All p = p

infixr 0 :->
type (:->) p q = p -> q

newtype K p = K{unK :: p}

