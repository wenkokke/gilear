{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Data.DeBruijn.Index.Inductive.Extra (
  SomeThinArgs (..),
  toSomeThinArgsRaw,
  SomeThickArgs (..),
  toSomeThickArgsRaw,
) where

import Control.DeepSeq (NFData (..))
import Data.DeBruijn.Index.Inductive (Ix, SomeIx (..), toSomeIxRaw)
import Data.DeBruijn.Index.Inductive.Arbitrary (arbitraryIx)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Type.Nat (Nat (..))
import Data.Type.Nat.Singleton.Inductive (SNat (..), SomeSNat (..), decSNat)
import Data.Type.Nat.Singleton.Inductive.Arbitrary ()
import GHC.Stack (HasCallStack)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen)

data SomeThinArgs
  = forall n. SomeThinArgs !(SNat n) !(Ix (S n)) !(Ix n)

toSomeThinArgsRaw :: (HasCallStack) => (Int, Int, Int) -> SomeThinArgs
toSomeThinArgsRaw (bound, index1, index2) =
  case toSomeIxRaw (succ bound, index1) of
    SomeIx{bound = sn, index = i} ->
      case toSomeIxRaw (bound, index2) of
        SomeIx{bound = n, index = j} ->
          case decSNat sn (S n) of
            Just Refl -> SomeThinArgs n i j
            Nothing -> error "toSomeThinArgsRaw: toSomeIxRaw failed to correctly convert bounds"

deriving instance Show SomeThinArgs

instance NFData SomeThinArgs where
  rnf :: SomeThinArgs -> ()
  rnf (SomeThinArgs n i j) = rnf n `seq` rnf i `seq` rnf j

instance Arbitrary SomeThinArgs where
  arbitrary :: Gen SomeThinArgs
  arbitrary = do
    SomeSNat n <- arbitrary
    i <- arbitraryIx (S (S n))
    j <- arbitraryIx (S n)
    pure $ SomeThinArgs (S n) i j

data SomeThickArgs
  = forall n. SomeThickArgs !(SNat n) !(Ix (S n)) !(Ix (S n))

toSomeThickArgsRaw :: (HasCallStack) => (Int, Int, Int) -> SomeThickArgs
toSomeThickArgsRaw (bound, index1, index2) =
  case toSomeIxRaw (bound, index1) of
    SomeIx{bound = S n, index = i} ->
      case toSomeIxRaw (bound, index2) of
        SomeIx{bound = S n', index = j} ->
          case decSNat n n' of
            Just Refl -> SomeThickArgs n i j
            Nothing -> error "toSomeThickArgs: toSomeIxRaw failed to correctly convert bounds"

deriving instance Show SomeThickArgs

instance NFData SomeThickArgs where
  rnf :: SomeThickArgs -> ()
  rnf (SomeThickArgs n i j) = rnf n `seq` rnf i `seq` rnf j

instance Arbitrary SomeThickArgs where
  arbitrary = do
    SomeSNat n <- arbitrary
    i <- arbitraryIx (S n)
    j <- arbitraryIx (S n)
    pure $ SomeThickArgs n i j
