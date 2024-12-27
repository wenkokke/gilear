{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Indexed where

import Data.Kind (Constraint, Type)

infixr 0 -->

type (-->) :: (k -> Type) -> (k -> Type) -> (k -> Type)
type (-->) p q i = p i -> q i

infixr 0 ==>

type (==>) :: (k -> Constraint) -> (k -> Type) -> (k -> Type)
type (==>) p q i = (p i) => q i

type K :: Type -> k -> Type
newtype K a i = K {unK :: a}

deriving stock instance (Show a) => Show (K a i)

type All :: (k -> Type) -> Type
type All p = forall i. p i

data Any (p :: k -> Type) :: Type where
  Any :: p i -> Any p

deriving stock instance (forall i. Show (p i)) => Show (Any p)

infixr 4 :*

data (:*) (p :: k -> Type) (q :: k -> Type) :: k -> Type where
  (:*) ::
    { fsti :: p i
    , sndi :: q i
    } ->
    (p :* q) i

deriving stock instance (Show (p i), Show (q i)) => Show ((p :* q) i)
