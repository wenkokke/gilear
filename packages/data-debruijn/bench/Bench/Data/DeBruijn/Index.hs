module Bench.Data.DeBruijn.Index (
  benchmarks,
) where

import Control.DeepSeq (NFData, force)
import Criterion.Main (Benchmark, Benchmarkable, bench, bgroup, nf)
import Data.DeBruijn.Index qualified as Unsafe (thin)
import Data.DeBruijn.Index.Extra qualified as Unsafe (SomeThinArgs (..), toSomeThinArgsRaw)
import Data.DeBruijn.Index.Inductive qualified as Inductive (thin)
import Data.DeBruijn.Index.Inductive.Extra qualified as Inductive (SomeThinArgs (..), toSomeThinArgsRaw)
import Text.Printf (printf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Bench.Data.DeBruijn.Index"
    [ bench_thin
    ]

bench_thin :: Benchmark
bench_thin =
  bgroup
    "bench_thin"
    [ bgroup "Unsafe" (bench_unsafeThinWith <$> thinArgsRaw)
    , bgroup "Inductive" (bench_inductiveThinWith <$> thinArgsRaw)
    ]
 where
  thinArgsRaw =
    [ -- TODO: some systematic non-random approach to inputs
      (4, 2, 3)
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
bench_thinWith toSomeThinArgs action thinArgsRaw@(n, i, j) = do
  let !benchLabel = printf "[%d,%d,%d]" n i j
  let !someThinArgs = force (toSomeThinArgs thinArgsRaw)
  bench benchLabel (action someThinArgs)
