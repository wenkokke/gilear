{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Data.Thinning.Unsafe (
  (:<) (Done, Keep, Drop),
  toBools,
  Thin (..),

  -- * Unsafe
  (:<) (UnsafeTh),
  ThRep (ThRep, size, bits),
) where

import Control.Exception (assert)
import Data.Bits (Bits (..))
import Data.Index.Unsafe (Ix (..), isPos)
import Data.Kind (Constraint, Type)
import Data.Type.Nat (Nat (..), Pos, Pred)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Thinning Representation
--------------------------------------------------------------------------------

data ThRep = ThRep
  { size :: {-# UNPACK #-} !Int
  , bits :: !Integer
  }
  deriving (Eq, Show)

isValidThRep :: ThRep -> Bool
isValidThRep th =
  th.size >= popCount th.bits

mkDoneRep :: ThRep
mkDoneRep =
  ThRep
    { size = 0
    , bits = zeroBits
    }
{-# INLINE mkDoneRep #-}

mkKeepRep :: ThRep -> ThRep
mkKeepRep th =
  assert (isValidThRep th) $
    ThRep
      { size = succ th.size
      , bits = th.bits
      }
{-# INLINE mkKeepRep #-}

mkDropRep :: ThRep -> ThRep
mkDropRep th =
  assert (isValidThRep th) $
    ThRep
      { size = succ th.size
      , bits = setBit th.bits th.size
      }
{-# INLINE mkDropRep #-}

getThRepChild :: ThRep -> ThRep
getThRepChild th =
  assert (isValidThRep th && th /= mkDoneRep) $
    let size' = pred th.size
    in  ThRep
          { size = size'
          , bits = clearBit th.bits size'
          }
{-# INLINE getThRepChild #-}

elThRep :: ThRep -> a -> (ThRep -> a) -> (ThRep -> a) -> a
elThRep th ifDone ifKeep ifDrop =
  assert (isValidThRep th && th /= mkDoneRep) $
    if th.size == 0
      then ifDone
      else
        if testBit th.bits (pred th.size)
          then ifKeep (getThRepChild th)
          else ifDrop (getThRepChild th)
{-# INLINE elThRep #-}

thinThRep :: ThRep -> ThRep -> ThRep
thinThRep th1 th2 =
  assert (th1.size >= th2.size) $
    ThRep
      { size = th1.size `max` th2.size
      , bits = th1.bits .|. shift th2.bits (th1.size - th2.size)
      }

_fromBitsThRep :: [Bool] -> ThRep
_fromBitsThRep th =
  ThRep
    { size = length th
    , bits = foldr (.|.) 0 (zipWith readBit [0 ..] th)
    }
 where
  readBit :: Int -> Bool -> Integer
  readBit _ False = 0
  readBit i True = bit i

_toBitsThRep :: ThRep -> [Bool]
_toBitsThRep th = flip fmap [0 .. th.size - 1] $ \case
  i
    | testBit th.bits i -> True
    | otherwise -> False

--------------------------------------------------------------------------------
-- Thinnings
--------------------------------------------------------------------------------

type (:<) :: Nat -> Nat -> Type
newtype (:<) n m = UnsafeTh {thRep :: ThRep}

type role (:<) nominal nominal

mkDone :: Z :< Z
mkDone = UnsafeTh mkDoneRep
{-# INLINE mkDone #-}

mkKeep :: n :< m -> S n :< S m
mkKeep = UnsafeTh . mkKeepRep . (.thRep)
{-# INLINE mkKeep #-}

mkDrop :: n :< m -> n :< S m
mkDrop = UnsafeTh . mkDropRep . (.thRep)
{-# INLINE mkDrop #-}

recTh :: n :< m -> a -> (Pred n :< Pred m -> a) -> (n :< Pred m -> a) -> a
recTh nm ifDone ifKeep ifDrop = elThRep nm.thRep ifDone (ifKeep . UnsafeTh) (ifDrop . UnsafeTh)
{-# INLINE recTh #-}

data ThF (th :: Nat -> Nat -> Type) (n :: Nat) (m :: Nat) where
  DoneF :: ThF th Z Z
  KeepF :: !(th n m) -> ThF th (S n) (S m)
  DropF :: !(th n m) -> ThF th n (S m)

projectTh :: n :< m -> ThF (:<) n m
projectTh nm = recTh nm (unsafeCoerce DoneF) (unsafeCoerce . KeepF) (unsafeCoerce . DropF)
{-# INLINE projectTh #-}

embedTh :: ThF (:<) n m -> n :< m
embedTh = \case
  DoneF -> mkDone
  KeepF n'm' -> mkKeep n'm'
  DropF nm' -> mkDrop nm'
{-# INLINE embedTh #-}

pattern Done :: () => (n ~ Z, m ~ Z) => n :< m
pattern Done <- (projectTh -> DoneF) where Done = embedTh DoneF
{-# INLINE Done #-}

pattern Keep :: () => (Pos n, Pos m) => Pred n :< Pred m -> n :< m
pattern Keep nm <- (projectTh -> KeepF nm) where Keep nm = embedTh (KeepF nm)
{-# INLINE Keep #-}

pattern Drop :: () => (Pos m) => n :< Pred m -> n :< m
pattern Drop nm <- (projectTh -> DropF nm) where Drop nm = embedTh (DropF nm)
{-# INLINE Drop #-}

{-# COMPLETE Done, Keep, Drop #-}

-- | Convert a thinning into a list of booleans.
toBools :: n :< m -> [Bool]
toBools (UnsafeTh th) = fmap (testBit th.bits) [0 .. th.size - 1]

--------------------------------------------------------------------------------
-- Thinning Class
--------------------------------------------------------------------------------

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
  thin nm ln = UnsafeTh (thinThRep nm.thRep ln.thRep)

  thick :: n :< m -> l :< m -> Maybe (l :< n)
  thick Done Done = Just Done
  thick (Keep n'm') (Keep l'n') = Keep <$> thick n'm' l'n'
  thick (Keep n'm') (Drop ln') = Drop <$> thick n'm' ln'
  thick (Drop _nm') (Keep _l'n') = Nothing
  thick (Drop nm') (Drop ln') = thick nm' ln'
