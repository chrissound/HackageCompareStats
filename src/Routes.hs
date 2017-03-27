{-# LANGUAGE OverloadedStrings #-}
module Routes where

import Common
import Web.Scotty.Trans
import Network.Wai (Request)
import Data.List (intercalate)
import Control.Monad.Trans.Reader
import Control.Monad.Trans

comparePackageProcessParams :: Maybe [Param] -> Maybe [Param]
comparePackageProcessParams = fmap . fmap $ f
  where f (p, _) = (("package[]"), p)

comparePackageRouteValidate :: String -> Request -> Maybe [Param]
comparePackageRouteValidate x = comparePackageProcessParams . Common.processParams x

comparePackageRouteMatcher :: String -> RoutePattern
comparePackageRouteMatcher = function . comparePackageRouteValidate

getURL :: [String] -> String
getURL = ((++) "/comparePackage/") . intercalate "/"

getExposeURL :: String -> ArchCompareActionM String
getExposeURL x = do
  archConfig <- lift ask
  return $ (++) (getBaseUrl archConfig) x
