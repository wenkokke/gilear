{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.DeBruijn.Thinning.Inductive (
  -- * Thinnings
  (:<=) (Done, Keep, Drop),
  toInductive,
  fromInductive,
  toBools,

  -- * Existential Wrapper
  SomeTh (..),
  fromBools,
  fromBits,
  fromBitsRaw,

  -- * The action of thinnings on 'Nat'-indexed types
  Thin (..),
) where

import Control.DeepSeq (NFData (..))
import Data.Bits (Bits (..))
import Data.DeBruijn.Index.Inductive (Ix (..), isPos)
import Data.DeBruijn.Thinning qualified as Efficient
import Data.Kind (Constraint, Type)
import Data.Type.Nat (Nat (..))
import Data.Type.Nat.Singleton.Inductive (SNat (..))

--------------------------------------------------------------------------------
-- Thinnings
--------------------------------------------------------------------------------

-- TODO:
-- Rewrite (:<=) using
-- @
--  Refl :: n :<= n
-- @
-- instead of @Done@.

-- | @n ':<=' m@ is the type of thinnings from @m@ to @n@.
type (:<=) :: Nat -> Nat -> Type
data (:<=) n m where
  Done :: Z :<= Z
  Keep :: n :<= m -> S n :<= S m
  Drop :: n :<= m -> n :<= S m

instance NFData (n :<= m) where
  rnf :: n :<= m -> ()
  rnf Done = ()
  rnf (Keep n'm') = rnf n'm'
  rnf (Drop nm') = rnf nm'

-- | Convert from the efficient representation 'Efficient.:<=' to the inductive representation ':<='.
toInductive :: n Efficient.:<= m -> n :<= m
toInductive Efficient.Done = Done
toInductive (Efficient.Keep n'm') = Keep (toInductive n'm')
toInductive (Efficient.Drop nm') = Drop (toInductive nm')

-- | Convert from the inductive representation ':<=' to the efficient representation 'Efficient.:<='.
fromInductive :: n :<= m -> n Efficient.:<= m
fromInductive Done = Efficient.Done
fromInductive (Keep n'm') = Efficient.Keep (fromInductive n'm')
fromInductive (Drop nm') = Efficient.Drop (fromInductive nm')

-- | Convert a thinning into a list of booleans.
toBools :: n :<= m -> [Bool]
toBools = \case
  Done -> []
  Keep n'm' -> True : toBools n'm'
  Drop nm' -> False : toBools nm'

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

data SomeTh
  = forall n m.
  SomeTh
  { lower :: SNat n
  , upper :: SNat m
  , value :: n :<= m
  }

instance NFData SomeTh where
  rnf :: SomeTh -> ()
  rnf SomeTh{..} = rnf lower `seq` rnf upper `seq` rnf value

emptySomeTh :: SomeTh
emptySomeTh =
  SomeTh
    { lower = Z
    , upper = Z
    , value = Done
    }

keepSomeTh :: SomeTh -> SomeTh
keepSomeTh SomeTh{..} =
  SomeTh
    { lower = S lower
    , upper = S upper
    , value = Keep value
    }

dropSomeTh :: SomeTh -> SomeTh
dropSomeTh SomeTh{..} =
  SomeTh
    { lower = lower
    , upper = S upper
    , value = Drop value
    }

fromBools :: [Bool] -> SomeTh
fromBools [] = emptySomeTh
fromBools (keepValue : rest)
  | keepValue = keepSomeTh (fromBools rest)
  | otherwise = dropSomeTh (fromBools rest)

fromBits :: (Integral i, Bits bs) => (i, bs) -> SomeTh
fromBits (upper, bits) = fromBools (testBit bits <$> [0 .. fromIntegral upper])

fromBitsRaw :: (Int, Integer) -> SomeTh
fromBitsRaw (upper, bits) = fromBools (testBit bits <$> [0 .. upper])

--------------------------------------------------------------------------------
-- Thinning Class
--------------------------------------------------------------------------------

-- | The actions of thinnings on natural-indexed data types.
type Thin :: (Nat -> Type) -> Constraint
class Thin f where
  thin :: n :<= m -> f n -> f m
  thick :: n :<= m -> f m -> Maybe (f n)

instance Thin Ix where
  thin :: n :<= m -> Ix n -> Ix m
  thin !t !i = isPos i $
    case t of
      Keep n'm' ->
        case i of
          FZ -> FZ
          FS i' -> FS (thin n'm' i')
      Drop nm' -> FS (thin nm' i)

  thick :: n :<= m -> Ix m -> Maybe (Ix n)
  thick Done _i = Nothing
  thick (Keep _n'm') FZ = Just FZ
  thick (Keep n'm') (FS i') = FS <$> thick n'm' i'
  thick (Drop _nm') FZ = Nothing
  thick (Drop nm') (FS i') = thick nm' i'

instance Thin ((:<=) l) where
  thin :: n :<= m -> l :<= n -> l :<= m
  thin Done Done = Done
  thin (Keep n'm') (Keep l'n') = Keep (thin n'm' l'n')
  thin (Keep n'm') (Drop ln') = Drop (thin n'm' ln')
  thin (Drop nm') ln = Drop (thin nm' ln)

  thick :: n :<= m -> l :<= m -> Maybe (l :<= n)
  thick Done Done = Just Done
  thick (Keep n'm') (Keep l'n') = Keep <$> thick n'm' l'n'
  thick (Keep n'm') (Drop ln') = Drop <$> thick n'm' ln'
  thick (Drop _nm') (Keep _l'n') = Nothing
  thick (Drop nm') (Drop ln') = thick nm' ln'
