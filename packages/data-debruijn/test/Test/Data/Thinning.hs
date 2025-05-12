module Test.Data.Thinning (
  tests,
) where

-- import Data.Thinning ()
-- import Data.Thinning.Inductive qualified as Unsafe (toInductive)
-- import Data.Thinning.Inductive qualified as Inductive
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck ()

tests :: TestTree
tests =
  testGroup
    "Test.Data.Thinning"
    []
