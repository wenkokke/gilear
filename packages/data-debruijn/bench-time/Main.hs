{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Bench.Time.Data.DeBruijn.Index qualified (benchmarks)
import Criterion.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.Time.Data.DeBruijn.Index.benchmarks
    ]
