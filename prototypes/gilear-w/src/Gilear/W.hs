{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gilear.W where

import Control.Category (Category (..))
import Control.Indexed (K (..), type All, type (-->))
import Control.Kripke (Box, Free (..), Kripke (step), MonadKripke (..), Path (..), World, walk)
import Control.Monad.Writer.Strict (MonadWriter (writer), Writer, runWriter)
import Data.DeBruijn (Ix (..), Nat (S, Z), thick, thin)
import Data.Function (on)
import Data.Kind (Type)
import Data.Monoid (Any (..))
import Data.Set (Set)
import Data.Text (Text)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (id, (.))

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

data TyF (t :: Type)
  = t :-> t
  | t :*  t
  | Bool
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Ty (n :: Nat)
  = E Name
  | U (Ix n)
  | C (TyF (Ty n))
  -- deriving (Eq, Show)

{-| Substitution for /existential/ variables.

  @'esub' r n t@ replaces @'E' n@ with @r@ in @t@.
-}
esub :: Ty Z -> Name -> Ty n -> Ty n
esub r m = go
 where
  go (E n) | m == n = closed r
  go (C t) = C (fmap go t)
  go t = t

{-| Conversion from /existential/ variable to /universal/ variable.

    @'eusub' i n t@ replaces @'E' n@ with @'U' i@ in @t@.
    The 'Writer' effect tracks whether or not @n@ occurred in @t@.
-}
eusub :: Ix (S n) -> Name -> Ty n -> Writer Any (Ty (S n))
eusub i m = go
 where
  go (E n)
    | m == n = writer (U i, Any True)
    | otherwise = pure (E n)
  go (U j) = pure (U (thin i j))
  go (C t) = C <$> traverse go t

{-| Substitution for /universal/ variables.

  @'usub' r i t@ replaces @'U' i@ with @r@ in @t@.
-}
usub :: Ty Z -> Ix (S n) -> Ty (S n) -> Ty n
usub r = go
 where
  go :: Ix (S n) -> Ty (S n) -> Ty n
  go _ (E n) = E n
  go j (U i) = maybe (closed r) U (thick j i)
  go j (C t) = C (fmap (go j) t)

newtype TyAt (w :: World) = TyAt (Ty Z)

data Scheme (n :: Nat) :: Type where
  Mono :: Ty n -> Scheme n
  Poly :: Scheme (S n) -> Scheme n

{-| Conversion from /existential/ variable to /universal/ variable.

    @'eusub' i n t@ replaces @'E' n@ with @'U' i@ in @t@.
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

{-| Generalisation of an /existential/ variable into a /universal/ variable.

 @'gen' n s@ generalises the /existential/ variable @n@ to a /universal/ variable in @s@.
-}
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

op :: (Kripke Sub p) => c p u -> Free Sub c p u
op c = Call c $ \_ p -> Pure p

instance Kripke Sub (K a) where
  step :: K a u -> Sub u w -> K a w
  step (K x) _ = K x

fresh :: Text -> Free Sub W (K Name) u
fresh text =
  op Next `kbind` \_uv p ->
    kpure (K $ Name text (unK (p Refl)))

-- handle Find...
decl :: (Kripke Sub p) => Name -> All (SchemeAt --> MonadW p --> MonadW p)
-- ...by giving the scheme if we're looking up this decl
decl n s (Call (Find m) k) | n == m = decl n s $ k Refl s
-- forward everything else, but...
decl _ _ (Pure p) = Pure p
-- ...be sure to update the scheme in the light of progress
decl n s (Call c k) = Call c $ \uv p -> decl n (walk s uv) $ k uv p

mkETyAt :: All (Box e (K Name) --> TyAt)
mkETyAt n = TyAt (E (unK (n Refl)))

bloc :: All (MonadW TyAt --> MonadW SchemeAt)
-- if we're instantiating a monotype, we're done
bloc (Call (Inst (SchemeAt (Mono t))) k) = bloc $ k Refl (TyAt t)
-- if we're instantiating a polytype, we're
-- inventing a fresh existential variable and guessing it
bloc (Call (Inst (SchemeAt (Poly s))) k) =
  fresh "x" `kbind` \uv n ->
    guess (n Refl) . bloc $
      op (Inst (inst s (mkETyAt n))) `kbind` \vw t ->
        k (vw . uv) (t Refl)
bloc (Pure (TyAt t)) = Pure (SchemeAt (Mono t))
-- otherwise forward
bloc (Call c k) = Call c $ \uv p -> bloc $ k uv p

-- handle Make, but also do generalisation (note we're computing type schemes)
guess :: All (K Name --> MonadW SchemeAt --> MonadW SchemeAt)
guess = undefined

-- -- when Make shows up, we have four possibilities
-- guessing (K e) (Call c@(Make ds (Ty t) x) k) = case (e == x, dep e t) of
--   -- (is it me?, do I occur in the definiens)
--   (True, True)  -- it's me and the occur check failed; oh noes!
--     -> op Barf
--   (True, False)  -- it's me, so extrude my dependencies and substitute me!
--     -> foldr (guessing . K) (k (Now :< (t :/: x)) (K ())) ds
--   (False, True)  -- it's not me, but I occur, so extrude me!
--     -> Call (Make (e : ds) (Ty t) x) k
--   (False, False)  -- it's nothing to do with me, so leave alone!
--     -> Call c $ \ u r -> guessing (K e) $ k u r
-- -- nobody made me; I could be anything; pawn becomes queen!
-- guessing e (RetNow s) = RetNow (gen e s)
-- -- forward the rest (the update is a no-op)
-- guessing (K e) (Call c k) = Call c $ \ u r -> guessing (K e) $ k u r
