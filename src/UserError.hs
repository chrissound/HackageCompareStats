{-# LANGUAGE OverloadedStrings #-}
module UserError where

import qualified Heist.Interpreted as I
import Heist.Internal.Types
import Data.String.Conversions
import Data.Map.Syntax
import CompareFormTemplate
import Common (PTitle, ArchCompareReadState)
import Data.Text (Text)
import Render

getErrorTmpl :: ArchCompareReadState -> String -> [PTitle]-> IO Text
getErrorTmpl archConfig e rP = (renderHeistTemplatePath"userError" $ errorBinds archConfig e rP)
  >>= (either (error. convertString) return )

errorBinds:: ArchCompareReadState -> String -> [PTitle] -> HeistState IO -> HeistState IO
errorBinds archConfig s rP = I.bindSplices $ do
  baseUrlBind archConfig
  "errorMessage"  ## I.textSplice . convertString $ s
  requestedPackageBind rP
