{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Bench.Space.Data.DeBruijn.Index qualified (benchmarks)
import Data.ByteString.Lazy qualified as BSL (putStr)
import Data.Csv (Header, NamedRecord, ToField (..), ToNamedRecord (..), encodeByName, header, namedRecord, (.=))
import Weigh (Column (..), Grouped (..), Weight (..), setColumns, weighResults)

main :: IO ()
main = do
  (groupedWeightsOrErrors, _config) <-
    weighResults $ do
      setColumns [Case, Allocated, Max]
      Bench.Space.Data.DeBruijn.Index.benchmarks
  let groupedWeights :: [Grouped Weight]
      groupedWeights = fmap (fmap fst) groupedWeightsOrErrors
  let results = ungroup =<< groupedWeights
  let csvData = encodeByName weightHeader (WeightRecord <$> results)
  BSL.putStr csvData

weightHeader :: Header
weightHeader =
  header
    [ "Label"
    , "AllocatedBytes"
    , "GCs"
    , "LiveBytes"
    , "MaxBytes"
    , "MaxOSBytes"
    , "WallTime"
    ]

newtype WeightRecord = WeightRecord Weight

instance ToNamedRecord WeightRecord where
  toNamedRecord :: WeightRecord -> NamedRecord
  toNamedRecord (WeightRecord Weight{..}) =
    namedRecord
      [ "Label" .= toField weightLabel
      , "AllocatedBytes" .= toField weightAllocatedBytes
      , "GCs" .= toField weightGCs
      , "LiveBytes" .= toField weightLiveBytes
      , "MaxBytes" .= toField weightMaxBytes
      , "MaxOSBytes" .= toField weightMaxOSBytes
      , "WallTime" .= toField weightWallTime
      ]

ungroup :: Grouped Weight -> [Weight]
ungroup (Grouped _label grouped) = ungroup =<< grouped
ungroup (Singleton weight) = [weight]
