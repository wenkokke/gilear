module GilearW where

import GilearW.Internal.Ix (Ix (..))

thin :: Ix -> Ix -> Ix
thin FZ y = FS y
thin (FS _) FZ = FZ
thin (FS x) (FS y) = FS (thin x y)

