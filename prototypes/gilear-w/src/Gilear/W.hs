module Gilear.W where

import Control.Kripke (World)
import Data.DeBruijn (Ix, Natural, S, Z)
import Data.Kind (Type)
import Data.Text (Text)

data Name = Name
  { text :: !Text
  , uniq :: !Word
  }

data Ty (n :: Natural) :: Type where
  E :: Name -> Ty n
  U :: Ix n -> Ty n
  (:->) :: Ty n -> Ty n -> Ty n

newtype TyAt (w :: World) = TyAt (Ty Z)

data Sch (n :: Natural) :: Type where
  T :: Ty n -> Sch n
  P :: Sch (S n) -> Sch n

newtype SchAt (w :: World) = SchAt (Sch Z)

data Sub (u :: World) (v :: World) = Sub
  { target :: Name
  , replacement :: Ty Z
  }
