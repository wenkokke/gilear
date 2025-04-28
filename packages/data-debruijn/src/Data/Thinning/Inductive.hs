{-# LANGUAGE GADTs #-}

module Data.Thinning.Inductive (
  (:<) (Done, Keep, Drop),
  Thin (..),
) where

import Data.Index.Inductive (Ix (..), isPos)
import Data.Kind (Constraint, Type)
import Data.Type.Nat (Nat (..))

--------------------------------------------------------------------------------
-- Thinnings
--------------------------------------------------------------------------------

-- | @'Th' m n@ is the type of thinnings from @m@ to @n@.
type (:<) :: Nat -> Nat -> Type
data (:<) n m where
  Done :: Z :< Z
  Keep :: !(n :< m) -> S n :< S m
  Drop :: !(n :< m) -> n :< S m

-- | The actions of thinnings on natural-indexed data types.
type Thin :: (Nat -> Type) -> Constraint
class Thin f where
  thin :: n :< m -> f n -> f m
  thick :: n :< m -> f m -> Maybe (f n)

instance Thin Ix where
  thin :: n :< m -> Ix n -> Ix m
  thin !t !i = isPos i $
    case t of
      Keep n'm' ->
        case i of
          FZ -> FZ
          FS i' -> FS (thin n'm' i')
      Drop nm' -> FS (thin nm' i)

  thick :: n :< m -> Ix m -> Maybe (Ix n)
  thick Done _i = Nothing
  thick (Keep _n'm') FZ = Just FZ
  thick (Keep n'm') (FS i') = FS <$> thick n'm' i'
  thick (Drop _nm') FZ = Nothing
  thick (Drop nm') (FS i') = thick nm' i'

instance Thin ((:<) l) where
  thin :: n :< m -> l :< n -> l :< m
  thin Done Done = Done
  thin (Keep n'm') (Keep l'n') = Keep (thin n'm' l'n')
  thin (Keep n'm') (Drop ln') = Drop (thin n'm' ln')
  thin (Drop nm') ln = Drop (thin nm' ln)

  thick :: n :< m -> l :< m -> Maybe (l :< n)
  thick Done Done = Just Done
  thick (Keep n'm') (Keep l'n') = Keep <$> thick n'm' l'n'
  thick (Keep n'm') (Drop ln') = Drop <$> thick n'm' ln'
  thick (Drop _nm') (Keep _l'n') = Nothing
  thick (Drop nm') (Drop ln') = thick nm' ln'
