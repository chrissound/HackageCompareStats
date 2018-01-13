{-# LANGUAGE OverloadedStrings #-}
module CompareFormTemplate where

import Data.Text (Text)
import qualified Text.XmlHtml as X
import qualified Heist.Interpreted as I
import qualified Arch
import Heist.Internal.Types
import Data.String.Conversions
import Data.Map.Syntax
import Data.String (IsString)
import           CompareFormJson                      (comparisonToHighChartSeries)
import           Text.Mustache
import           Render
import           Data.Aeson                           (encode)
import           Control.Monad
import           Prelude
import           Common
import qualified Data.ByteString as Str
import System.Directory (doesFileExist, getModificationTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.List (intercalate)
import Text.XmlHtml

type FeaturedHtml = [Node]

optionHtml :: Text -> X.Node
optionHtml v = X.Element "option" [("value", v)] []

baseUrl :: Text -> X.Node
baseUrl v = X.Element "base" [("href", v)] []

generateRecordSplice :: Text -> I.Splice IO
generateRecordSplice = I.runNode . optionHtml

generateRecordSplices :: [Text] -> I.Splice IO
generateRecordSplices = I.mapSplices generateRecordSplice

requestedPackageBind :: (Data.String.IsString k, Show a, Monad m) => a -> MapSyntax k (HeistT n m Heist.Internal.Types.Template)
requestedPackageBind x = "requestedPackage" ## (I.textSplice . convertString $ show x)

packagesBind :: IsString k => MapSyntax k (I.Splice IO)
packagesBind = "packages" ## I.textSplice "{{{cachePackages}}}"

preparePackagesCache :: Arch.PackagesStats -> IO ()
preparePackagesCache x = do
  -- A quick caching solution... Else it's really slow
  -- It's a bit of a hack for now
  fileExist <- doesFileExist packagesCachePath
  if not fileExist then
   prepare
  else do
    lmTime <- getModificationTime packagesCachePath
    time <- getCurrentTime
    let timeDiff = abs $ diffUTCTime lmTime time
        nominalDay = 60 * 60 * 24 :: NominalDiffTime in
        when (timeDiff > nominalDay) $ prepare
  where
    prepare = do
      hRender <- renderHeistTemplatePath "cachePackages" (I.bindSplices $ "packages" ## generateRecordSplices . fmap fst $ Arch.getPackages x)
      case hRender of
        Right hRender' -> Str.writeFile packagesCachePath $ convertString hRender'
        Left e -> error $ convertString e

compareFormFormBinds :: ArchCompareReadState -> FeaturedHtml -> HeistBind
compareFormFormBinds archConfig featured = I.bindSplices $ do
  baseUrlBind archConfig
  packagesBind
  "featured" ## return featured

baseUrlBind :: IsString k => ArchCompareReadState -> MapSyntax k (I.Splice IO)
baseUrlBind archConfig = "baseUrl" ## I.runNode (baseUrl . convertString $ getBaseUrl archConfig)

requestedPackagesVsBind :: IsString k => [PTitle] -> MapSyntax k (I.Splice IO)
requestedPackagesVsBind x = "requestedPackagesVs" ## I.textSplice txt
  where txt = convertString $ intercalate " VS " (convertString <$> x)

compareFormBinds :: ArchCompareReadState -> [PTitle] -> FeaturedHtml-> HeistBind
compareFormBinds archConfig rp featured = do
  I.bindSplices $ do
    packagesBind
    requestedPackagesVsBind rp
    baseUrlBind archConfig
    "statisticResult" ## I.runChildrenWith (requestedPackageBind rp)
    "featured" ## return featured
 
packagesCachePath :: FilePath
packagesCachePath = "templates/cache/packages.tpl"

getFeaturedHtml :: IO [Node]
getFeaturedHtml = do
      featured <- readFile "templates/heist/featured.tpl"
      case parseHTML "featured" (cs featured) of
            Right x -> return $ docContent x
            Left e -> return $ [TextNode $ cs $"Not able to parse HTML from templates/heist/featured.tpl: " ++ e]

getComparePackageTmpl :: [PTitle] -> [APS] -> ArchCompareReadState -> IO Text
getComparePackageTmpl requestedPackages' foundPackages archConfig = do
      preparePackagesCache $ getStore archConfig
      scriptInject <- readFile "templates/compareForm.js"
      let jsonData = (comparisonToHighChartSeries $ convert foundPackages)
      let mRender = renderMustacheTemplate scriptInject
                      (object ["jsonData" ~> (convertString $ encode jsonData :: String)])
      featured <- getFeaturedHtml
      case mRender of
        Just mRender' -> do
          packagesCache <- Str.readFile packagesCachePath
          let mBinding = object [
                "scriptInject" ~> mRender'
               ,"cachePackages" ~> (convertString $ packagesCache :: Text)
                ]
          renderTemplate "compareForm" ( compareFormBinds archConfig requestedPackages' featured) $ Just mBinding
        Nothing -> error "Mustache render error. Failed to render "

getComparePackageFormTmpl :: ArchCompareReadState -> IO Text
getComparePackageFormTmpl archConfig = do
  featured <- getFeaturedHtml
  renderTemplate "compareFormForm" (compareFormFormBinds archConfig featured) Nothing
