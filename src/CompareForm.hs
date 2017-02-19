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

comparePackageHandler :: ActionM ()
comparePackageHandler = do
  requestedPackages' <- requestedPackages
  rescue (do
    when ( not $ length requestedPackages' >= 2) $
      raise "You need to specify atleast two requestedPackages"
    statisticsStore <- liftIO $ Arch.getPackagesStats "packageStatistics.json"
    case (statisticsStore) of
      Just statisticsStore' -> do
        case comparePackageGetPackages requestedPackages' statisticsStore' of
          Right aps -> (lift $ getComparePackageTmpl requestedPackages' aps statisticsStore') >>= (html . convertString)
          Left e -> raise . convertString $ e
      Nothing -> raise "Couldn't open database store"
    ) (\e-> (lift $ getErrorTmpl (convertString e) requestedPackages') >>= (html . convertString))

comparePackageFormHandler :: ActionM ()
comparePackageFormHandler = do
  rescue (do
    statisticsStore <- liftIO $ Arch.getPackagesStats "packageStatistics.json"
    withStatisticStore (\store -> (lift $ getComparePackageTmpl [] [] store) >>= (html . convertString)) statisticsStore
    ) (\e-> (lift $ getErrorTmpl (convertString e) []) >>= (html . convertString))


catchError :: [PTitle] -> String -> ActionM ()
catchError pkgs = (\e -> (lift $ getErrorTmpl (convertString e) pkgs) >>= (html . convertString))

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
