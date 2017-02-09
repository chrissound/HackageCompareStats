{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad
import Control.Monad.Trans
import Prelude
import qualified Arch
import Data.Text (Text)
--import qualified Data.Text
import Data.String.Conversions
import Data.Maybe (isJust)

import Text.Mustache

import CompareForm
import Common
import Render


comparePackage :: ActionM ()
comparePackage = do
  let blazeBinding = (object ["scriptInject" ~> ("<h1>woah</h1>" :: String)])
  requestedPackages <- comparePackageRequestedPackages
  statisticsStore <- liftIO $ Arch.getPackagesStats "packageStatistics.json"
  case (statisticsStore) of
    Just statisticsStore' -> do
      aps <- comparePackageGetPackages requestedPackages statisticsStore'
      --_ <- renderTemplate "compareForm"  $ compareFormBinds statisticsStore' requestedPackages aps
      --renderTemplate "compareForm"  id
      rendered <- lift $ renderTemplate "compareForm"  ( compareFormBinds statisticsStore' requestedPackages aps ) blazeBinding
      html . convertString $ rendered
    Nothing -> raise "Couldn't open database store"


comparePackageRequestedPackages :: ActionM [Text]
comparePackageRequestedPackages = do
  requestedPackages <- multiParam "package[]"
  when ( not $ length requestedPackages >= 2) $
    raise "You need to specify atleast two requestedPackages"
  return requestedPackages

comparePackageGetPackages :: [Text] -> APSm -> ActionM [APS]
comparePackageGetPackages requestedPackages statisticsStore = do
  let searchPackages = Arch.searchPackageStats statisticsStore
  let packagesResult = map (searchPackages . convertString) requestedPackages :: [Maybe APS]
  case (sequence packagesResult) of
    Just (results) -> return results
    Nothing -> raise $ convertString $ "Unable to find the following requestedPackages: "  ++ show packagesNotFound where
      packagesNotFound = join $ zipWith
        (\requestedPkg packageResult -> if isJust packageResult then [] else [requestedPkg])
        requestedPackages packagesResult


main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/comparePackage" comparePackage
