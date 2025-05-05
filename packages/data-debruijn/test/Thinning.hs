module Thinning (tests) where

import Data.Thinning
import qualified Data.Thinning.Inductive as Ind
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Thinning tests"
    [ -- testProperty "toBoolsEq" toBoolsEq
    ]

-- toBoolsEq :: (Testable a) => n :< m -> a
-- toBoolsEq t = toBools t == Ind.toBools (Ind.toInductive t)
