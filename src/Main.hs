{-# LANGUAGE OverloadedStrings #-}
module Main  where

import           Prelude
import           CompareForm
import           Web.Scotty.Trans
import qualified Arch
import Common
import Control.Monad.Trans.Reader
import System.Environment (getArgs)

getBaseUrlArg :: IO String
getBaseUrlArg = do
  args <- getArgs
  case args of
    (arg:[]) -> return arg
    _ -> error "Base url arg not passed"

main :: IO ()
main = do
  baseURl <- getBaseUrlArg
  statisticsStore <- Arch.getPackagesStats "packageStatistics.json"
  case statisticsStore of
    Just y -> do
      let readState = ArchCompareReadState baseURl y
      scottyT 3000 (\x -> runReaderT x readState) $ do
       get (literal "/comparePackage") $ comparePackageHandler
       get "/comparePackage" $ comparePackageFormHandler
       get "" $ comparePackageFormHandler
    Nothing -> error "Unable to find data store!"
