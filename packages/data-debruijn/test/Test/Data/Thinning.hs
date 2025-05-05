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
    "Thinning tests"
    []

-- testProperty "toBoolsEq" toBoolsEq

-- toBoolsEq :: (Testable a) => n :< m -> a
-- toBoolsEq t = toBools t == Ind.toBools (Ind.toInductive t)
