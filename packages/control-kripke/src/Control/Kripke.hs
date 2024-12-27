{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Kripke where

import Control.Category (Category (..))
import Data.Kind (Type)
import Prelude hiding (id, (.))

--------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

--------------------------------------------------------------------------------

type All :: (k -> Type) -> Type
type All p = forall w. p w

infixr 0 -->

type (-->) :: (k -> Type) -> (k -> Type) -> (k -> Type)
type (-->) p q x = p x -> q x

data World :: Type

type Path :: (World -> World -> Type) -> World -> World -> Type
data Path e u v where
  Refl :: Path e u u
  Step :: e u v -> Path e u v
  Trans :: Path e u v -> Path e v w -> Path e u w

instance Category (Path e) where
  id :: Path e u u
  id = Refl
  (.) :: Path e v w -> Path e u v -> Path e u w
  (.) = flip Trans

class
  Kripke
    (e :: World -> World -> Type)
    (p :: World -> Type)
    | p -> e
  where
  -- | Step a value along an edge.
  step :: p u -> e u w -> p w

  -- | Walk a value along a path.
  walk :: p u -> Path e u w -> p w
  walk p Refl = p
  walk p (Step uw) = step p uw
  walk p (Trans uv vw) = walk (walk p uv) vw

instance Kripke e (Path e u) where
  step :: Path e u v -> e v w -> Path e u w
  step uv vw = Trans uv (Step vw)

-- | The /necessity/ modality from modal logic.
type Box :: (World -> World -> Type) -> (World -> Type) -> World -> Type
type Box e p u = forall w. Path e u w -> p w

box :: (Kripke e p) => All (p --> Box e p)
box p uv = walk p uv

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
    Free e c p u ->
    e u v ->
    Free e c p v
  step (Pure p) uv =
    Pure (step p uv)
  step (Call c k) uv =
    Call (step c uv) $ \vw w -> k (vw . Step uv) w

  walk ::
    ( forall (q :: World -> Type).
      Kripke e (c q)
    , Kripke e p
    ) =>
    Free e c p u ->
    Path e u v ->
    Free e c p v
  walk (Pure p) uv =
    Pure (walk p uv)
  walk (Call c k) uv =
    Call (walk c uv) $ \vw q -> k (vw . uv) q

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
