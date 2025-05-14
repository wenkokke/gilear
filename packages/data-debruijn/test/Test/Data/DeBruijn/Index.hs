module Test.Data.DeBruijn.Index (tests) where

import Data.DeBruijn.Index qualified as Unsafe
import Data.DeBruijn.Index.Inductive (SomeIx (bound, index))
import Data.DeBruijn.Index.Inductive qualified as Inductive
import Data.DeBruijn.Index.Inductive qualified as Unsafe (fromInductive, toInductive)
import Data.DeBruijn.Index.Inductive.Arbitrary ()
import Data.DeBruijn.Index.Inductive.Extra qualified as Inductive (SomeThickArgs (..), SomeThinArgs (..))
import Data.Proxy (Proxy (..))
import Data.Type.Nat.Singleton.Inductive qualified as Inductive (SomeSNat (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Test.DeBruijn.Data.Index"
    [ testProperty "test_eqIxEq" test_eqIxEq
    , testProperty "test_fromIxRawEq" test_fromIxRawEq
    , testProperty "test_fromIxEq" test_fromIxEq
    , testProperty "test_injectEq" test_injectEq
    , testProperty "test_thinEq" test_thinEq
    , testProperty "test_thickEq" test_thickEq
    ]

test_eqIxEq :: Inductive.SomeIx -> Inductive.SomeIx -> Bool
test_eqIxEq (Inductive.SomeIx{index = i, bound = _}) (Inductive.SomeIx{index = j, bound = _}) =
  Inductive.eqIx i j == Unsafe.eqIx (Unsafe.fromInductive i) (Unsafe.fromInductive j)

test_fromIxRawEq :: Inductive.SomeIx -> Bool
test_fromIxRawEq (Inductive.SomeIx{index = i, bound = _}) =
  Inductive.fromIxRaw i == Unsafe.fromIxRaw (Unsafe.fromInductive i)

test_fromIxEq :: Inductive.SomeIx -> Bool
test_fromIxEq (Inductive.SomeIx{index = i, bound = _}) =
  Inductive.fromIx @Int i == Unsafe.fromIx @Int (Unsafe.fromInductive i)

test_injectEq :: Inductive.SomeSNat -> Inductive.SomeIx -> Bool
test_injectEq (Inductive.SomeSNat n) (Inductive.SomeIx{index = i, bound = _}) =
  Inductive.inject n i == Unsafe.toInductive (Unsafe.inject (erase n) (Unsafe.fromInductive i))

test_thinEq :: Inductive.SomeThinArgs -> Bool
test_thinEq (Inductive.SomeThinArgs _n i j) =
  Inductive.thin i j == Unsafe.toInductive (Unsafe.thin (Unsafe.fromInductive i) (Unsafe.fromInductive j))

test_thickEq :: Inductive.SomeThickArgs -> Bool
test_thickEq (Inductive.SomeThickArgs _n i j) =
  Inductive.thick i j == (Unsafe.toInductive <$> Unsafe.thick (Unsafe.fromInductive i) (Unsafe.fromInductive j))

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | @`erase` x@ erases the content of @x@ to a @`Proxy`@.
erase :: f a -> Proxy a
erase _ = Proxy
{-# INLINE erase #-}
