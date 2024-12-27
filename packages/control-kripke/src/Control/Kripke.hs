{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Kripke where

import Control.Category (Category (..))
import Data.Kind (Type)
import Prelude hiding (id, (.))

type All :: (k -> Type) -> Type
type All p = forall w. p w

infixr 0 -->

type (-->) :: (k -> Type) -> (k -> Type) -> (k -> Type)
type (-->) p q x = p x -> q x

data World :: Type

type Path :: (World -> World -> Type) -> World -> World -> Type
data Path e u v where
  Refl :: Path e u u
  Step :: Path e u v -> e v w -> Path e u w

singleton :: e v w -> Path e v w
singleton = Step Refl

trans :: Path e u v -> Path e v w -> Path e u w
trans uv Refl = uv
trans uv (Step vt vw) = Step (trans uv vt) vw

instance Category (Path e) where
  id :: Path e u u
  id = Refl
  (.) :: Path e v w -> Path e u v -> Path e u w
  (.) = flip trans

class
  Kripke
    (e :: World -> World -> Type)
    (p :: World -> Type)
    | p -> e
  where
  -- | Step a value along an edge.
  step :: e u w -> p u -> p w

  -- | Move a value along a path.
  move :: Path e u w -> p u -> p w
  move Refl p = p
  move (Step uv vw) p = step vw (move uv p)

-- | The /necessity/ modality from modal logic.
type Box :: (World -> World -> Type) -> (World -> Type) -> World -> Type
type Box e p u = forall w. Path e u w -> p w

box :: (Kripke e p) => All (p --> Box e p)
box p uv = move uv p

class
  MonadKripke
    (e :: World -> World -> Type)
    (m :: (World -> Type) -> World -> Type)
  where
  kpure ::
    (Kripke e p) =>
    All (p --> m p)

  kbind ::
    (Kripke e p, Kripke e q) =>
    m p u ->
    (forall v. Path e u v -> Box e p v -> m q v) ->
    m q u

  kdrop ::
    (Kripke e p, Kripke e q) =>
    m p u ->
    (forall v. Path e u v -> m q v) ->
    m q u
  kdrop mpu mqv = mpu `kbind` \uv _ -> mqv uv

data
  Free
    (e :: World -> World -> Type)
    (c :: (World -> Type) -> World -> Type)
    (p :: World -> Type)
    (u :: World) ::
    Type
  where
  Pure ::
    forall e c p u.
    p u ->
    Free e c p u
  Call ::
    forall e c p q u.
    c q u ->
    All (Path e u --> q --> Free e c p) ->
    Free e c p u

instance
  ( forall q. Kripke e (c q)
  , Kripke e p
  ) =>
  Kripke e (Free e c p)
  where
  step ::
    ( forall (q :: World -> Type).
      Kripke e (c q)
    , Kripke e p
    ) =>
    e u v ->
    Free e c p u ->
    Free e c p v
  step uv (Pure p) =
    Pure (step uv p)
  step uv (Call c k) =
    Call (step uv c) $ \vw w -> k (vw . singleton uv) w

  move ::
    ( forall (q :: World -> Type).
      Kripke e (c q)
    , Kripke e p
    ) =>
    Path e u v ->
    Free e c p u ->
    Free e c p v
  move uv (Pure p) =
    Pure (move uv p)
  move uv (Call c k) =
    Call (move uv c) $ \vw q -> k (vw . uv) q

instance MonadKripke e (Free e c) where
  kpure ::
    (Kripke e p) =>
    All (p --> Free e c p)
  kpure = Pure

  kbind ::
    (Kripke e p, Kripke e q) =>
    Free e c p u ->
    ( forall (v :: World).
      Path e u v ->
      Box e p v ->
      Free e c q v
    ) ->
    Free e c q u
  Pure p `kbind` k =
    k Refl (box p)
  Call c j `kbind` k =
    Call c $ \uw wr ->
      j uw wr `kbind` \vw ->
        k (vw . uw)
