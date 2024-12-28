{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Gilear.W where

import Control.Indexed (K (..), type All, type (-->))
import Control.Kripke (Free (..), Kripke (step), Path (..), World)
import Control.Monad.Writer.Strict (MonadWriter (writer), Writer, runWriter)
import Data.DeBruijn (Ix (..), Nat (S, Z), thick, thin)
import Data.Function (on)
import Data.Kind (Type)
import Data.Monoid (Any (..))
import Data.Set (Set)
import Data.Text (Text)
import Unsafe.Coerce (unsafeCoerce)

data Name = Name
  { text :: !Text
  , uniq :: !Word
  }

instance Eq Name where
  (==) :: Name -> Name -> Bool
  (==) = (==) `on` uniq

instance Ord Name where
  compare :: Name -> Name -> Ordering
  compare = compare `on` uniq

data Ty (n :: Nat) :: Type where
  EVar :: Name -> Ty n
  UVar :: Ix n -> Ty n
  (:->) :: Ty n -> Ty n -> Ty n

{-| Substitution for /existential/ variables.

  @'esub' r n t@ replaces @'EVar' n@ with @r@ in @t@.
-}
esub :: Ty Z -> Name -> Ty n -> Ty n
esub r m = go
 where
  go (EVar n) | m == n = closed r
  go (s :-> t) = go s :-> go t
  go t = t

{-| Conversion from /existential/ variable to /universal/ variable.

    @'eusub' i n t@ replaces @'EVar' n@ with @'UVar' i@ in @t@.
    The 'Writer' effect tracks whether or not @n@ occurred in @t@.
-}
eusub :: Ix (S n) -> Name -> Ty n -> Writer Any (Ty (S n))
eusub i m = go
 where
  go (EVar n)
    | m == n = writer (UVar i, Any True)
    | otherwise = pure (EVar n)
  go (UVar j) = pure (UVar (thin i j))
  go (s :-> t) = (:->) <$> go s <*> go t

{-| Substitution for /universal/ variables.

  @'usub' r i t@ replaces @'UVar' i@ with @r@ in @t@.
-}
usub :: Ty Z -> Ix (S n) -> Ty (S n) -> Ty n
usub r = go
 where
  go :: Ix (S n) -> Ty (S n) -> Ty n
  go _ (EVar n) = EVar n
  go j (UVar i) = maybe (closed r) UVar (thick j i)
  go j (s :-> t) = go j s :-> go j t

newtype TyAt (w :: World) = TyAt (Ty Z)

data Scheme (n :: Nat) :: Type where
  Mono :: Ty n -> Scheme n
  Poly :: Scheme (S n) -> Scheme n

{-| Conversion from /existential/ variable to /universal/ variable.

    @'eusub' i n t@ replaces @'EVar' n@ with @'UVar' i@ in @t@.
    The 'Writer' effect tracks whether or not @n@ occurred in @t@.
-}
eusubScheme :: Ix (S n) -> Name -> Scheme n -> Writer Any (Scheme (S n))
eusubScheme i m = go i
 where
  go :: Ix (S n) -> Scheme n -> Writer Any (Scheme (S n))
  go j (Mono t) = Mono <$> eusub j m t
  go j (Poly s) = Poly <$> go (FS j) s

-- | Substitution for /existential/ variables in type schemes.
esubScheme :: Ty Z -> Name -> Scheme n -> Scheme n
esubScheme r n = go
 where
  go :: Scheme n -> Scheme n
  go (Mono t) = Mono (esub r n t)
  go (Poly t) = Poly (go t)

-- | Substitution for /universal/ variables in type schemes.
usubScheme :: Ty Z -> Ix (S n) -> Scheme (S n) -> Scheme n
usubScheme r = go
 where
  go :: Ix (S n) -> Scheme (S n) -> Scheme n
  go i (Mono t) = Mono (usub r i t)
  go i (Poly t) = Poly (go (FS i) t)

newtype SchemeAt (w :: World) = SchemeAt (Scheme Z)

data Sub (u :: World) (v :: World) = Sub (Ty Z) Name

-- | TODO: Enable coercion from @'Ix' n@ into @'Ix' m@ when @n < m@.
closed :: Ty Z -> Ty n
closed = unsafeCoerce

instance Kripke Sub TyAt where
  step :: TyAt u -> Sub u w -> TyAt w
  step (TyAt t) (Sub r e) = TyAt (esub r e t)

instance Kripke Sub SchemeAt where
  step :: SchemeAt u -> Sub u w -> SchemeAt w
  step (SchemeAt s) (Sub r e) = SchemeAt (esubScheme r e s)

{-| Instantiation of the top-most /universal/ variable.

    @'inst' s t@ instantates the top-most quantifier in @s@ with @t@.
-}
inst :: Scheme (S Z) -> All (TyAt --> SchemeAt)
inst s (TyAt t) = SchemeAt (usubScheme t FZ s)

-- | Generalisation of an /existential/ variable into a /universal/ variable.
--
--  @'gen' n s@ generalises the /existential/ variable @n@ to a /universal/ variable in @s@.
gen :: Name -> All (SchemeAt --> SchemeAt)
gen n (SchemeAt s) =
  case runWriter (eusubScheme FZ n s) of
    (s', hit) -> SchemeAt $ if getAny hit then Poly s' else s

---------------
-- Effects
---------------

type MonadW :: (World -> Type) -> World -> Type
type MonadW p u = Free Sub W p u

data W (p :: World -> Type) (u :: World) where
  Next :: W (K Word) u
  Find :: Name -> W SchemeAt u
  Inst :: SchemeAt u -> W TyAt u
  Make :: Set Name -> TyAt u -> Name -> W (K ()) u
  Fail :: W f u

instance Kripke Sub (W p) where
  step :: W p u -> Sub u w -> W p w
  step Next _ = Next
  step (Find x) _ = Find x
  step (Inst s) w = Inst (step s w)
  step (Make deps t x) w = Make deps (step t w) x
  step Fail _ = Fail

supply :: (Kripke Sub p) => Word -> All (MonadW p --> MonadW p)
supply n (Call Next k) = supply (n + 1) $ k Refl (K n)
supply _ p@(Pure _) = p
supply n (Call c k) = Call c $ \uv q -> supply n $ k uv q
