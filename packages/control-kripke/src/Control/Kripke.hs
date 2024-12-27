{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Kripke where

import Control.Category (Category (..))
import Data.Kind (Type)
import Prelude hiding (id, (.))

type All :: (k -> Type) -> Type
type All f = forall x. f x

infixr 0 -->

type (-->) :: (k -> Type) -> (k -> Type) -> (k -> Type)
type (-->) f g x = f x -> g x

data World :: Type

type Path :: (World -> World -> Type) -> World -> World -> Type
data Path step r t where
  Refl :: Path step r r
  Step :: Path step r s -> step s t -> Path step r t

compose :: Path step s t -> Path step r s -> Path step r t
compose Refl prs = prs
compose (Step psu ut) prs = Step (compose psu prs) ut

instance Category (Path step) where
  id :: Path step a a
  id = Refl
  (.) :: Path step b c -> Path step a b -> Path step a c
  (.) = compose

class
  Kripke
    (step :: World -> World -> Type)
    (v :: World -> Type)
    | v -> step
  where
  -- | Update a 'Kripke' value with a single 'step'.
  updateWithStep :: step r t -> v r -> v t

  -- | Update a 'Kripke' value with a 'Path'.
  update :: Path step r t -> v r -> v t
  update Refl v = v
  update (Step prs st) v = updateWithStep st (update prs v)

type Box :: (World -> World -> Type) -> (World -> Type) -> World -> Type
type Box step v s = forall t. Path step s t -> v t

kripke :: (Kripke step v) => All (v --> Box step v)
kripke v pst = update pst v

class
  MonadKripke
    (step :: World -> World -> Type)
    (m :: (World -> Type) -> World -> Type)
  where
  tpure ::
    (Kripke step v) =>
    All (v --> m v)

  tbind ::
    (Kripke step v, Kripke step w) =>
    m v s ->
    (forall t. Path step s t -> Box step v t -> m w t) ->
    m w s

  tdrop ::
    (Kripke step v, Kripke step w) =>
    m v s ->
    (forall t. Path step s t -> m w t) ->
    m w s
  tdrop mv mw = mv `tbind` \pst _ -> mw pst

data
  Free
    (step :: World -> World -> Type)
    (c :: (World -> Type) -> World -> Type)
    (v :: World -> Type)
    (s :: World) ::
    Type
  where
  Pure ::
    forall step c v s.
    v s ->
    Free step c v s
  Call ::
    forall step c v w s.
    c w s ->
    All (Path step s --> w --> Free step c v) ->
    Free step c v s

instance
  ( forall w. Kripke step (c w)
  , Kripke step v
  ) =>
  Kripke step (Free step c v)
  where
  updateWithStep ::
    (forall (w :: World -> Type). Kripke step (c w), Kripke step v) =>
    step r s ->
    Free step c v r ->
    Free step c v s
  updateWithStep rs (Pure v) =
    Pure (updateWithStep rs v)
  updateWithStep rs (Call c k) =
    Call (updateWithStep rs c) $ \pst w -> k (pst . Step Refl rs) w

  update ::
    (forall (w :: World -> Type). Kripke step (c w), Kripke step v) =>
    Path step r s ->
    Free step c v r ->
    Free step c v s
  update prs (Pure v) =
    Pure (update prs v)
  update prs (Call c k) =
    Call (update prs c) $ \pst w -> k (pst . prs) w

instance MonadKripke step (Free step c) where
  tpure ::
    (Kripke step v) =>
    All (v --> Free step c v)
  tpure = Pure

  tbind ::
    (Kripke step v, Kripke step w) =>
    Free step c v s ->
    ( forall (t :: World).
      Path step s t ->
      Box step v t ->
      Free step c w t
    ) ->
    Free step c w s
  Pure v `tbind` k =
    k Refl (kripke v)
  Call c j `tbind` k =
    Call c $ \psr wr ->
      j psr wr `tbind` \prt ->
        k (prt . psr)
