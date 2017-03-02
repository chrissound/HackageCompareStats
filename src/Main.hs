{-# LANGUAGE OverloadedStrings #-}
module Main  where

import           Prelude
import           CompareForm
import           Web.Scotty.Trans
import qualified Arch
import Common
import Control.Monad.Trans.Reader

main :: IO ()
main = do
  statisticsStore <- Arch.getPackagesStats "packageStatistics.json"
  case statisticsStore of
    Just y -> do
      let readState = ArchCompareReadState "test" y
      scottyT 3000 (\x -> runReaderT x readState) $ do
       get (literal "/comparePackage") $ comparePackageHandler
       get "/comparePackage" $ comparePackageFormHandler
       get "" $ comparePackageFormHandler
    Nothing -> error "Unable to find data store!"
