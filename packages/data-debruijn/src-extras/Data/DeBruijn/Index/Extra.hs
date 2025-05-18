{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Data.DeBruijn.Index.Extra (
  SomeThinArgs (..),
  toSomeThinArgsRaw,
  fromInductiveSomeThinArgs,
  SomeThickArgs (..),
  toSomeThickArgsRaw,
  fromInductiveSomeThickArgs,
) where

import Control.DeepSeq (NFData (..))
import Data.DeBruijn.Index (Ix)
import Data.DeBruijn.Index.Inductive qualified as Ix.Unsafe (fromInductive)
import Data.DeBruijn.Index.Inductive.Extra qualified as Inductive
import Data.Type.Nat (Nat (..))
import Data.Type.Nat.Singleton (SNat (..))
import Data.Type.Nat.Singleton.Inductive qualified as SNat.Unsafe (fromInductive)

data SomeThinArgs
  = forall n. SomeThinArgs !(SNat n) !(Ix (S n)) !(Ix n)

toSomeThinArgsRaw :: (Int, Int, Int) -> SomeThinArgs
toSomeThinArgsRaw = fromInductiveSomeThinArgs . Inductive.toSomeThinArgsRaw

fromInductiveSomeThinArgs :: Inductive.SomeThinArgs -> SomeThinArgs
fromInductiveSomeThinArgs (Inductive.SomeThinArgs n i j) =
  SomeThinArgs (SNat.Unsafe.fromInductive n) (Ix.Unsafe.fromInductive i) (Ix.Unsafe.fromInductive j)

deriving instance Show SomeThinArgs

instance NFData SomeThinArgs where
  rnf :: SomeThinArgs -> ()
  rnf (SomeThinArgs n i j) = rnf n `seq` rnf i `seq` rnf j

data SomeThickArgs
  = forall n. SomeThickArgs !(SNat n) !(Ix (S n)) !(Ix (S n))

toSomeThickArgsRaw :: (Int, Int, Int) -> SomeThickArgs
toSomeThickArgsRaw = fromInductiveSomeThickArgs . Inductive.toSomeThickArgsRaw

fromInductiveSomeThickArgs :: Inductive.SomeThickArgs -> SomeThickArgs
fromInductiveSomeThickArgs (Inductive.SomeThickArgs n i j) =
  SomeThickArgs (SNat.Unsafe.fromInductive n) (Ix.Unsafe.fromInductive i) (Ix.Unsafe.fromInductive j)

deriving instance Show SomeThickArgs

instance NFData SomeThickArgs where
  rnf :: SomeThickArgs -> ()
  rnf (SomeThickArgs n i j) = rnf n `seq` rnf i `seq` rnf j
