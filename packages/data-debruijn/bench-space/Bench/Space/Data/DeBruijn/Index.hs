module Bench.Space.Data.DeBruijn.Index where

import Control.DeepSeq (force)
import Data.DeBruijn.Index qualified as Unsafe
import Data.DeBruijn.Index.Extra qualified as Unsafe
import Data.DeBruijn.Index.Inductive qualified as Inductive
import Data.DeBruijn.Index.Inductive.Extra qualified as Inductive
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (nub)
import Text.Printf (printf)
import Weigh (Weigh, func', wgroup)

benchmarks :: Weigh ()
benchmarks =
  wgroup "Bench.Data.DeBruijn.Index" $ do
    bench_thin
    bench_thick

--------------------------------------------------------------------------------
-- Benchmark: thin
--------------------------------------------------------------------------------

bench_thin :: Weigh ()
bench_thin =
  wgroup "thin" $ do
    wgroup "Unsafe" $ do
      traverse_ bench_unsafeThinWith thinArgsRawList
    wgroup "Inductive" $ do
      traverse_ bench_inductiveThinWith thinArgsRawList

bench_unsafeThinWith :: (Int, Int, Int) -> Weigh ()
bench_unsafeThinWith thinArgsRaw@(_n, u, v) = do
  let !benchLabel = printf "[%d,%d]" u v
  case force (Unsafe.toSomeThinArgsRaw thinArgsRaw) of
    (Unsafe.SomeThinArgs _n i j) ->
      func' benchLabel (Unsafe.thin i) j

bench_inductiveThinWith :: (Int, Int, Int) -> Weigh ()
bench_inductiveThinWith thinArgsRaw@(_n, u, v) = do
  let !benchLabel = printf "[%d,%d]" u v
  case force (Inductive.toSomeThinArgsRaw thinArgsRaw) of
    (Inductive.SomeThinArgs _n i j) ->
      func' benchLabel (Inductive.thin i) j

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

bench_thick :: Weigh ()
bench_thick =
  wgroup "thick" $ do
    wgroup "Unsafe" $ do
      traverse_ bench_unsafeThickWith thickArgsRawList
    wgroup "Inductive" $ do
      traverse_ bench_inductiveThickWith thickArgsRawList

bench_unsafeThickWith :: (Int, Int, Int) -> Weigh ()
bench_unsafeThickWith thickArgsRaw@(_n, u, v) = do
  let !benchLabel = printf "[%d,%d]" u v
  case force (Unsafe.toSomeThickArgsRaw thickArgsRaw) of
    (Unsafe.SomeThickArgs _n i j) ->
      func' benchLabel (Unsafe.thick i) j

bench_inductiveThickWith :: (Int, Int, Int) -> Weigh ()
bench_inductiveThickWith thickArgsRaw@(_n, u, v) = do
  let !benchLabel = printf "[%d,%d]" u v
  case force (Inductive.toSomeThickArgsRaw thickArgsRaw) of
    (Inductive.SomeThickArgs _n i j) ->
      func' benchLabel (Inductive.thick i) j

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
