{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Gilear.W where

import Control.Indexed (K (..), type All, type (-->))
import Control.Kripke (Box, Free (..), Kripke (..), MonadKripke (..), Path (..), World, box)
import Data.Bifunctor (bimap, first)
import Data.DeBruijn (Ix (FS, FZ), Nat (S, Z), inject, thick, thin)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text (Text)
import Unsafe.Coerce (unsafeCoerce)

data Name = Name
  { text :: !Text
  , uniq :: !Word
  }
  deriving (Eq, Ord)

data Ty (n :: Nat) :: Type where
  ExVar :: Name -> Ty n
  UnVar :: Ix n -> Ty n
  (:->) :: Ty n -> Ty n -> Ty n

newtype TyAt (w :: World) = TyAt (Ty Z)

data Scheme (n :: Nat) :: Type where
  Mono :: Ty n -> Scheme n
  Poly :: Scheme (S n) -> Scheme n

newtype SchemeAt (w :: World) = SchemeAt (Scheme Z)

data Subst (u :: World) (v :: World) = Subst
  { target :: Name
  , replacement :: Ty Z
  }

closed :: f Z -> f n
closed = unsafeCoerce

subst :: Subst u v -> Ty n -> Ty n
subst Subst{..} = go
 where
  go (ExVar name) | target == name = closed replacement
  go (s :-> t) = go s :-> go t
  go t = t

instance Kripke Subst TyAt where
  step (TyAt t) s = TyAt (subst s t)

instance Kripke Subst SchemeAt where
  step (SchemeAt sch) s = SchemeAt (go sch)
    where
      go :: All (Scheme --> Scheme)
      go (Mono t) = Mono (subst s t)
      go (Poly t) = Poly (go t)

-- -- instantiation
-- stan :: Scheme (S Z) -> All (TyAt --> SchemeAt)
-- stan sch (TyAt t) = SchemeAt (sub FZ sch)
--   where
--     sub :: Ix (S n) -> Scheme (S n) -> Scheme n
--     sub j (Mono t') = Mono (go t')
--       where
--         go :: Ty (S n) -> Ty (S n)
--         go e@(ExVar _) = e
--         go (UnVar i)   = case thick j i of
--           Nothing -> closed t
--           Just i' -> UnVar i'
--         go (t1 :-> t2) = go t1 :-> go t2
--     sub j (Poly t') = Poly (sub (FS j) t')

-- generalisation
-- gen :: All (K Name --> SchemeAt --> SchemeAt)
-- gen (K e) (Sch sch) =
--   case go FZ sch of
--     (sch, True)  -> Scheme (Poly sch)
--     (_  , False) -> Scheme sch
--   where
--     go :: Ix (S n) -> Scheme n -> (Scheme (S n), Bool)
--     go j (Mono ty) = first Mono (euTy e j ty)
--     go j (Poly ty) = first Poly (go (FS j) ty)

--     euTy :: Name -> Ix (S n) -> Ty n -> (Ty (S n), Bool)
--     euTy e j (ExVar x) =
--       if e == x then (UnVar j, True) else (ExVar x, False)
--     euTy _ j (UnVar i) = (UnVar (thin j i), False)
--     euTy e j (s :-> t) = bimap (:->) (||) (euTy e j s, euTy e j t)

---------------
-- Effects
---------------

data W (p :: World -> Type) (u :: World) where
  Next :: W (K Word) u
  VarScheme :: Name -> W SchemeAt u
  Instantiate :: SchemeAt u -> W TyAt u
  Define :: Set Name -> TyAt u -> Name -> W (K ()) u
  BarfAllOver :: W f u -- HardBarf for now

-- instance Kripke Subst (W p) where
--   step Next _ = Next
--   step (VarScheme x) _ = VarScheme x
--   step (Instantiate s) w = Instantiate (step s w)
--   step (Define setOfDependencies tyAtW x) w =
--     Define setOfDependencies (step tyAtW w) x
--   step BarfAllOver _ = BarfAllOver

-- type FreeW p u = Free Subst W p u
-- type SubKripke p = Kripke Subst p

-- supply :: SubKripke p => Word -> All (FreeW p --> FreeW p)
-- supply n (Call Next k) = supply (n + 1) $ k Refl (K n)
-- supply _ p@(Pure _) = p
-- supply n (Call c k) = Call c $ \ uv q -> supply n $ k uv q
