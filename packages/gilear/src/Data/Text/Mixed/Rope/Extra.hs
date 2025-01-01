module Data.Text.Mixed.Rope.Extra where

import Data.Text (Text)
import Data.Text.Lines (Position)
import Data.Text.Mixed.Rope (Rope)
import Data.Text.Mixed.Rope qualified as Rope

charAt :: Position -> Rope -> Text
charAt = ((Rope.toText . fst . Rope.charSplitAt 1 . snd) .) . Rope.charSplitAtPosition
