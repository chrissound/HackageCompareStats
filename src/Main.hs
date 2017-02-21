{-# LANGUAGE OverloadedStrings #-}
module Main  where

import           Prelude
import           CompareForm
import           Web.Scotty
import qualified Arch

main :: IO ()
main = do
  -- Great candidate for reader monad. I'm not there yet however.
  statisticsStore <- Arch.getPackagesStats "packageStatistics.json"
  case statisticsStore of
    Just store -> scotty 3000 $ do
      get (literal "/comparePackage") $ comparePackageHandler store
      get "/comparePackage" $ comparePackageFormHandler store
      get "" $ comparePackageFormHandler store
    Nothing -> error "Unable to find data store!"
