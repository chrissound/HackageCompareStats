{-# LANGUAGE OverloadedStrings #-}
module UserError where

import qualified Heist.Interpreted as I
import Heist.Internal.Types
import Data.String.Conversions
import Data.Map.Syntax
import CompareFormTemplate
import Common (PTitle)
import Data.Text (Text)
import Render

getErrorTmpl :: String -> [PTitle]-> IO Text
getErrorTmpl e rP = (renderHeistTemplatePath"userError" $ errorBinds e rP)
  >>= (either (error. convertString) return )

errorBinds:: String -> [PTitle] -> HeistState IO -> HeistState IO
errorBinds s rP = I.bindSplices $ do
  "errorMessage"  ## I.textSplice . convertString $ s
  requestedPackageBind rP
