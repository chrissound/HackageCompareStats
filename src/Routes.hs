{-# LANGUAGE OverloadedStrings #-}
module Routes where

import Common
import Web.Scotty.Trans
import Network.Wai (Request)
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Data.Binary.Builder
import Network.HTTP.Types
import Data.Monoid
import Data.String.Conversions
import Data.List

comparePackageProcessParams :: Maybe [Param] -> Maybe [Param]
comparePackageProcessParams = fmap . fmap $ f
  where f (p, _) = (("package[]"), p)

comparePackageRouteValidate :: String -> Request -> Maybe [Param]
comparePackageRouteValidate x = comparePackageProcessParams . Common.processParams x

comparePackageRouteMatcher :: String -> RoutePattern
comparePackageRouteMatcher = function . comparePackageRouteValidate

getURL :: [String] -> String
getURL x = convertString . toLazyByteString $
  encodePathSegments ["comparePackage"] <> encodePathSegments (convertString <$> x)

getExposeURL :: String -> ArchCompareActionM String
getExposeURL x = flip (<>)  x <$> (lift ask >>= (return . dropWhileEnd (== '/') .getBaseUrl))
