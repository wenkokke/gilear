{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.DeBruijn.Environment.Unsafe (
  -- * Environments
  Env (Nil, (:>)),

  -- * Unsafe
  Env (UnsafeEnv),
) where

import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Type.Nat (Nat (..), Pos, Pred)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Environment Representation
--------------------------------------------------------------------------------

type Env :: Nat -> Type -> Type
newtype Env n a = UnsafeEnv {envRep :: Seq a}

type EnvF :: (Nat -> Type -> Type) -> Nat -> Type -> Type
data EnvF env n a where
  NilF :: EnvF env Z a
  SnocF :: env n a -> a -> EnvF env (S n) a

projectEnv :: Env n a -> EnvF Env n a
projectEnv env = case Seq.viewr env.envRep of
  Seq.EmptyR -> unsafeCoerce NilF
  xs Seq.:> x -> unsafeCoerce SnocF xs x

embedEnv :: EnvF Env n a -> Env n a
embedEnv = \case
  NilF -> UnsafeEnv Seq.empty
  SnocF xs x -> UnsafeEnv (xs.envRep Seq.:|> x)

pattern Nil :: () => (n ~ Z) => Env n a
pattern Nil <- (projectEnv -> NilF) where Nil = embedEnv NilF
{-# INLINE Nil #-}

pattern (:>) :: () => (Pos n) => Env (Pred n) a -> a -> Env n a
pattern (:>) xs x <- (projectEnv -> SnocF xs x) where (:>) xs x = embedEnv (SnocF xs x)
{-# INLINE (:>) #-}

{-# COMPLETE Nil, (:>) #-}
