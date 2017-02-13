{-# LANGUAGE DeriveGeneric #-}
module Common where

import qualified Arch
import Data.String.Conversions
import Data.Text (Text)
import Web.Scotty
import Data.List (sort)
import GHC.Generics

type PackageTitle = Text
type PTitle = PackageTitle
type APS = Arch.PackageStat
type APSs = Arch.PackagesStats
newtype APCSm = APCSm [(Text, Float)] deriving (Show, Generic)

data PackageStatComparison = PackageStatComparison {
  comparison :: Maybe APS,
  packages :: APCSm
} deriving (Show)

convert :: [APS] -> PackageStatComparison
convert x@(_:_) = PackageStatComparison (Just $ topPkg) $ APCSm sorted where
  sorted@(topPkg:_) = fmap convertAPS $ sort x where
    (_, topPerc) = topPkg
    convertAPS (p, perc) = (p, perc / topPerc)
convert _ = PackageStatComparison Nothing $ APCSm []

multiParam :: Text -> ActionM [Text]
multiParam x = do
  v <- params
  return $ convertString . snd <$> (filter (\z -> (fst z == convertString x)) $ v)
