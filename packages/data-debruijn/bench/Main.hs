{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Bench.Data.DeBruijn.Index qualified (benchmarks)
import Criterion.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.Data.DeBruijn.Index.benchmarks
    ]
