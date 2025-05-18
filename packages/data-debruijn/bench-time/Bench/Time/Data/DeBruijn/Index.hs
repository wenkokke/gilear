module Bench.Time.Data.DeBruijn.Index (
  benchmarks,
) where

import Control.DeepSeq (NFData, force)
import Criterion.Main (Benchmark, Benchmarkable, bench, bgroup, nf)
import Data.DeBruijn.Index qualified as Unsafe
import Data.DeBruijn.Index.Extra qualified as Unsafe
import Data.DeBruijn.Index.Inductive qualified as Inductive
import Data.DeBruijn.Index.Inductive.Extra qualified as Inductive
import Data.Functor ((<&>))
import Data.List (nub)
import Text.Printf (printf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Bench.Data.DeBruijn.Index"
    [ bench_thin
    , bench_thick
    ]

--------------------------------------------------------------------------------
-- Benchmark: thin
--------------------------------------------------------------------------------

bench_thin :: Benchmark
bench_thin =
  bgroup
    "thin"
    [ bgroup "Unsafe" (bench_unsafeThinWith <$> thinArgsRawList)
    , bgroup "Inductive" (bench_inductiveThinWith <$> thinArgsRawList)
    ]

bench_unsafeThinWith :: (Int, Int, Int) -> Benchmark
bench_unsafeThinWith =
  bench_thinWith Unsafe.toSomeThinArgsRaw $ \(Unsafe.SomeThinArgs _n i j) ->
    nf (Unsafe.thin i) j

bench_inductiveThinWith :: (Int, Int, Int) -> Benchmark
bench_inductiveThinWith =
  bench_thinWith Inductive.toSomeThinArgsRaw $ \(Inductive.SomeThinArgs _n i j) ->
    nf (Inductive.thin i) j

bench_thinWith ::
  (NFData someThinArgs) =>
  ((Int, Int, Int) -> someThinArgs) ->
  (someThinArgs -> Benchmarkable) ->
  (Int, Int, Int) ->
  Benchmark
bench_thinWith toSomeThinArgs action thinArgsRaw@(_n, i, j) = do
  let !benchLabel = printf "[%d,%d]" i j
  let !someThinArgs = force (toSomeThinArgs thinArgsRaw)
  bench benchLabel (action someThinArgs)

thinArgsRawList :: [(Int, Int, Int)]
thinArgsRawList = nub (varyingParameter0 <> varyingParameter1)
 where
  varyingParameter0 =
    [ (101, i, j)
    | i <- [0, 10 .. 100]
    , j <- [0, i - 1, i, i + 1, 100]
    , 0 <= j && j < 101
    ]
  varyingParameter1 =
    varyingParameter0
      <&> (\(n, j, i) -> (n, i, j))

--------------------------------------------------------------------------------
-- Benchmark: thick
--------------------------------------------------------------------------------

bench_thick :: Benchmark
bench_thick =
  bgroup
    "thick"
    [ bgroup "Unsafe" (bench_unsafeThickWith <$> thickArgsRawList)
    , bgroup "Inductive" (bench_inductiveThickWith <$> thickArgsRawList)
    ]

bench_unsafeThickWith :: (Int, Int, Int) -> Benchmark
bench_unsafeThickWith =
  bench_thickWith Unsafe.toSomeThickArgsRaw $ \(Unsafe.SomeThickArgs _n i j) ->
    nf (Unsafe.thick i) j

bench_inductiveThickWith :: (Int, Int, Int) -> Benchmark
bench_inductiveThickWith =
  bench_thickWith Inductive.toSomeThickArgsRaw $ \(Inductive.SomeThickArgs _n i j) ->
    nf (Inductive.thick i) j

bench_thickWith ::
  (NFData someThickArgs) =>
  ((Int, Int, Int) -> someThickArgs) ->
  (someThickArgs -> Benchmarkable) ->
  (Int, Int, Int) ->
  Benchmark
bench_thickWith toSomeThickArgs action thickArgsRaw@(_n, i, j) = do
  let !benchLabel = printf "[%d,%d]" i j
  let !someThickArgs = force (toSomeThickArgs thickArgsRaw)
  bench benchLabel (action someThickArgs)

thickArgsRawList :: [(Int, Int, Int)]
thickArgsRawList = nub (varyingParameter0 <> varyingParameter1)
 where
  varyingParameter0 =
    [ (101, i, j)
    | i <- [0, 10 .. 100]
    , j <- [0, i - 1, i, i + 1, 100]
    , 0 <= j && j < 101
    ]
  varyingParameter1 =
    varyingParameter0
      <&> (\(n, j, i) -> (n, i, j))
