{-# LANGUAGE OverloadedStrings #-}
module Main  where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger
import Control.Monad
import Control.Monad.Trans
import Prelude
import qualified Arch
import Data.Text (Text)
import Data.String.Conversions
import Data.Maybe (isJust)

import Text.Mustache

import CompareForm
import CompareFormJson (comparisonToHighChartSeries)
import Common
import Render
import Data.Aeson (encode)

getComparePackageTmpl :: [PTitle] -> [APS] -> APSs-> IO Text
getComparePackageTmpl requestedPackages' foundPackages statisticsStore = do
      scriptInject <- readFile "templates/compareForm.js"
      let jsonData2 = (comparisonToHighChartSeries $ convert foundPackages)
      let mRender = renderMustacheTemplate scriptInject
                      (object ["jsonData" ~> (convertString $ encode jsonData2 :: String)])
      case mRender of
        Just mRender' -> do
          let blazeBinding = object ["scriptInject" ~> mRender']
          renderTemplate "compareForm" ( compareFormBinds statisticsStore requestedPackages' foundPackages ) blazeBinding
        Nothing -> error "Mustache render error. Failed to render "

comparePackageHandler :: ActionM ()
comparePackageHandler = do
  requestedPackages' <- requestedPackages
  statisticsStore <- liftIO $ Arch.getPackagesStats "packageStatistics.json"
  case (statisticsStore) of
    Just statisticsStore' -> do
      case comparePackageGetPackages requestedPackages' statisticsStore' of
        Right aps -> do
          render <- lift $ getComparePackageTmpl requestedPackages' aps statisticsStore'
          html . convertString $ render
        Left e -> raise . convertString $ e
    Nothing -> raise "Couldn't open database store"


requestedPackages :: ActionM [PTitle]
requestedPackages = do
  requestedPackages' <- multiParam "package[]"
  when ( not $ length requestedPackages' >= 2) $
    raise "You need to specify atleast two requestedPackages"
  return requestedPackages'

comparePackageGetPackages :: [PTitle] -> APSs -> Either String [APS]
comparePackageGetPackages requestedPackages' statisticsStore = do
  let searchPackages = Arch.searchPackageStats statisticsStore
  let packagesResult = map (searchPackages . convertString) requestedPackages' :: [Maybe APS]
  case (sequence packagesResult) of
    Just (results) -> return results
    Nothing -> Left . convertString $ "Unable to find the following requestedPackages: "  ++ show packagesNotFound where
      packagesNotFound = join $ zipWith
        (\requestedPkg packageResult -> if isJust packageResult then [] else [requestedPkg])
        requestedPackages' packagesResult

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/comparePackage" comparePackageHandler
