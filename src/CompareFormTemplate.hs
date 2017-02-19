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

optionHtml :: Text -> X.Node
optionHtml v = X.Element "option" [("value", v)] []

generateRecordSplice :: Text -> I.Splice IO
generateRecordSplice = I.runNode . optionHtml

generateRecordSplices :: [Text] -> I.Splice IO
generateRecordSplices = I.mapSplices generateRecordSplice

requestedPackageBind :: (Data.String.IsString k, Show a, Monad m) => a -> MapSyntax k (HeistT n m Heist.Internal.Types.Template)
requestedPackageBind x = "requestedPackage" ## (I.textSplice . convertString $ show x)

compareFormBinds :: Show a => Arch.PackagesStats -> [a] -> [APS] -> HeistState IO -> HeistState IO
compareFormBinds x' rp' results = I.bindSplices $
                        do "packages" ##
                             (generateRecordSplices . fmap fst $ Arch.getPackages x')
                           "statisticResult" ##
                             I.runChildrenWith
                               (do
                                   requestedPackageBind rp'
                                   "statisticRemark" ##
                                     (I.textSplice .
                                      convertString .
                                      concat . statisticRemark . reverse . sortPs $
                                      results))

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

statisticRemark :: [APS] -> [String]
statisticRemark ( (xhead,xp) : xs@(xshead,xsp) : xss ) =
  [convertString xhead ++ " is " ++ show (xp / xsp) ++ "x as popular as " ++ convertString xshead ++ ". "]
  ++ statisticRemark (xs:xss)
statisticRemark _ = []
