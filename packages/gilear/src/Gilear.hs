{-# LANGUAGE DerivingStrategies #-}

module Gilear
  ( TC,
    runTC,
    TCEnv,
    newTCEnv,
  ) where

import Gilear.Internal.TC (TC, runTC, TCEnv, newTCEnv)
