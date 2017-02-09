{-# LANGUAGE OverloadedStrings #-}
module CompareForm where

import Data.Text (Text)
import qualified Text.XmlHtml as X
import qualified Heist.Interpreted as I
import qualified Arch
import Heist.Internal.Types
import Data.List (sortBy)
import Data.String.Conversions
import Data.Map.Syntax

import Common

sortPs :: [APS] -> [APS]
sortPs ps = sortBy (\(_,x) (_,y) -> compare x y) ps

optionHtml :: Text -> X.Node
optionHtml v = X.Element "option" [("value", v)] []

generateRecordSplice :: Text -> I.Splice IO
generateRecordSplice = I.runNode . optionHtml

generateRecordSplices :: [Text] -> I.Splice IO
generateRecordSplices = I.mapSplices generateRecordSplice

statisticRemark :: [APS] -> [String]
statisticRemark ( (xhead,xp) : xs@(xshead,xsp) : xss ) =
  [convertString xhead ++ " is " ++ show (xp / xsp) ++ "x as popular as " ++ convertString xshead ++ ". "]
  ++ statisticRemark (xs:xss)
statisticRemark _ = []

compareFormBinds :: Show a => Arch.PackagesStats -> [a] -> [APS] -> HeistState IO -> HeistState IO
compareFormBinds x' rp' results = I.bindSplices $
                        do "packages" ##
                             (generateRecordSplices . fmap fst $ Arch.getPackages x')
                           "statisticResult" ##
                             I.runChildrenWith
                               (do "requestedPackage" ##
                                     (I.textSplice . convertString $ show rp')
                                   "statisticRemark" ##
                                     (I.textSplice .
                                      convertString .
                                      concat . statisticRemark . reverse . sortPs $
                                      results))
