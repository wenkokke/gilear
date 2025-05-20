module Test.Data.DeBruijn.Thinning (
  tests,
) where

-- import Data.DeBruijn.Thinning ()
-- import Data.DeBruijn.Thinning.Inductive qualified as Unsafe (toInductive)
-- import Data.DeBruijn.Thinning.Inductive qualified as Inductive
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck ()

tests :: TestTree
tests =
  testGroup
    "Test.Data.DeBruijn.Thinning"
    []
