module Test.Data.Thinning (
  tests,
) where

import Data.Thinning ()
-- import Data.Thinning.Inductive qualified as Th (toInductive)
-- import Data.Thinning.Inductive qualified as Th.Inductive
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck ()

tests :: TestTree
tests =
  testGroup
    "Test.Data.Thinning"
    []

-- testProperty "test_toBoolsEq" test_toBoolsEq

-- test_toBoolsEq :: (Testable a) => n :<= m -> a
-- test_toBoolsEq t = toBools t == Ind.toBools (Ind.toInductive t)
