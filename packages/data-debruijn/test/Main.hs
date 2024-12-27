import Data.DeBruijn (SomeIx (SomeIx), inject, raise, toNatural)
import Data.DeBruijn.Arbitrary (SomeSNat (SomeSNat))
import GHC.TypeNats (fromSNat)
import Test.QuickCheck (quickCheck)

prop_injectIsIdentity :: SomeSNat -> SomeIx -> Bool
prop_injectIsIdentity (SomeSNat m) (SomeIx _bound value) =
  toNatural (inject m value) == toNatural value

prop_raiseIsAdd :: SomeSNat -> SomeIx -> Bool
prop_raiseIsAdd (SomeSNat m) (SomeIx _bound value) =
  toNatural (raise m value) == fromSNat m + toNatural value

main :: IO ()
main = do
  quickCheck prop_injectIsIdentity
  quickCheck prop_raiseIsAdd
