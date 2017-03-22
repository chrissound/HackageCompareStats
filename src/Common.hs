{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Common where

import qualified Arch
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text.Internal.Lazy as LText (Text)
import Web.Scotty.Trans (ScottyT, ActionT, params, html)
import GHC.Generics
import Data.List (sortBy, reverse)
import Control.Monad.Trans.Reader
import Data.Function (on)

type PackageTitle = Text
type PTitle = PackageTitle
type APS = Arch.PackageStat
type APSs = Arch.PackagesStats
newtype APCSm = APCSm [Arch.PackageStat] deriving (Show, Generic)

data ArchCompareReadState = ArchCompareReadState
  { getBaseUrl :: String
  , getStore :: APSs
  }
type ArchCompareScottyM = ScottyT LText.Text (ReaderT ArchCompareReadState IO)
type ArchCompareActionM = ActionT LText.Text (ReaderT ArchCompareReadState IO)

data PackageStatComparison = PackageStatComparison {
  comparison :: Maybe APS,
  packages :: APCSm
} deriving (Show)

convert :: [APS] -> PackageStatComparison
convert x@(_:_) = PackageStatComparison (Just $ topPkg) $ APCSm sorted where
  sorted@(topPkg:_) = reverse $ sortBy (compare `on` snd) x
convert _ = PackageStatComparison Nothing $ APCSm []

multiParam :: Monad m => Text -> ActionT LText.Text m [PTitle]
multiParam x = do
  v <- params
  return $ convertString . snd <$> (filter (\z -> (fst z == convertString x)) $ v)

sortPs :: [APS] -> [APS]
sortPs ps = sortBy (\(_,x) (_,y) -> compare x y) ps


bob :: Text -> Text
bob x = x <> (convertString "wooo")

respondHtml :: (Monad m, ConvertibleStrings a LText.Text) => a -> ActionT LText.Text m ()
respondHtml = html . convertString
