{-# LANGUAGE DerivingStrategies #-}

module Gilear
  ( TC,
    runTC,
    TCEnv,
    newTCEnv,
  ) where

import Gilear.Internal.Core (TC, runTC, TCEnv, newTCEnv)
