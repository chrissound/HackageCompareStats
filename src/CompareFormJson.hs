{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module CompareFormJson where

import Data.Aeson
import Data.Text (Text)
import Common
import Data.String.Conversions
import GHC.Generics
import Data.Aeson.Types
import qualified Data.String (IsString)

instance ToJSON APCSm where
    toEncoding = genericToEncoding defaultOptions

data HighChartsSet a = HighChartsSet {
    name :: String
  , jdata :: [a]
} deriving (Generic)
data HighChartsSeries a = HighChartsSeries {
  series :: [HighChartsSet a]
} deriving (Generic)

highChartSetLabelModifier :: (Data.String.IsString t, Eq t) => t -> t
highChartSetLabelModifier x
  | x == "jdata" = "data"
  | otherwise    = x

instance (ToJSON a) => ToJSON (HighChartsSet a) where
   toEncoding = genericToEncoding $ defaultOptions { fieldLabelModifier = highChartSetLabelModifier}
instance (ToJSON a) => ToJSON (HighChartsSeries a) where
   toEncoding = genericToEncoding $ defaultOptions

comparisonToHighChartSeries :: PackageStatComparison -> HighChartsSeries Int
comparisonToHighChartSeries (PackageStatComparison _ apcsm) = do
  HighChartsSeries $ fmap (convertPackagesToSeries) x
  where
    (APCSm x) = apcsm
    convertPackagesToSeries :: (Text, a) -> HighChartsSet a
    convertPackagesToSeries (pkgName, score) = HighChartsSet (convertString pkgName) [score]
