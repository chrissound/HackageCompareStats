{-# LANGUAGE OverloadedStrings #-}
module CompareForm where

import qualified Arch
import           Common
import           CompareFormTemplate
import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe                           (isJust)
import           Data.String.Conversions
import           Prelude
import           UserError                            (getErrorTmpl)
import           Web.Scotty
import qualified Data.Text.Internal.Lazy as LText (Text)

comparePackageHandler :: ActionM ()
comparePackageHandler = do
  requestedPackages' <- requestedPackages
  rescue (do
    when ( not $ length requestedPackages' >= 2) $
      raise "You need to specify atleast two requestedPackages"
    statisticsStore <- liftIO $ Arch.getPackagesStats "packageStatistics.json"
    withStatisticStore
      (\store -> case comparePackageGetPackages requestedPackages' store of
          Right aps -> (lift $ getComparePackageTmpl requestedPackages' aps store) >>= respondHtml
          Left e -> raise . convertString $ e
      )
      statisticsStore
    ) (catchError requestedPackages')

comparePackageFormHandler :: ActionM ()
comparePackageFormHandler = do
  rescue (do
    statisticsStore <- liftIO $ Arch.getPackagesStats "packageStatistics.json"
    withStatisticStore
      (\store -> (lift $ getComparePackageTmpl [] [] store) >>= (respondHtml))
      statisticsStore
    ) (catchError [])


catchError :: [PTitle] -> (LText.Text-> ActionM ())
catchError pkgs = (\e -> (lift $ getErrorTmpl (convertString e) pkgs) >>= respondHtml)

withStatisticStore :: (APSs -> ActionM ()) -> Maybe APSs -> ActionM ()
withStatisticStore = maybe (raise "Couldn't open database store")

requestedPackages :: ActionM [PTitle]
requestedPackages = multiParam "package[]" >>= return . filter (/= "")

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
