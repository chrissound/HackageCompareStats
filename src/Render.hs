{-# LANGUAGE OverloadedStrings #-}
module Render where

import qualified Heist
import Data.String.Conversions
import Data.ByteString.Builder (toLazyByteString)
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as HeistCom
import Text.Mustache (compileTemplate, substituteValue)
import Data.Map.Syntax
import Heist.Internal.Types
import Data.Text (Text)
import Text.Mustache.Types (Value)

renderMustacheTemplate :: String -> Value -> Text
renderMustacheTemplate name binding = case (compileTemplate "" (convertString name)) of
  Right template -> substituteValue template binding
  Left _ -> error $ "Failed to render " ++ name


renderTemplate :: String -> (HeistState IO -> HeistState IO) -> Value -> IO Text
renderTemplate fileName hsBinding blazeBinding= do
  let emptyI = return () :: MapSyntax Text (I.Splice IO)
  let emptyC = return () :: MapSyntax Text (HeistCom.Splice IO)
  let emptyA = return () :: MapSyntax Text (AttrSplice IO)
  let templateLocations = [Heist.loadTemplates "templates/"]
  let spliceConfig = SpliceConfig emptyI emptyI emptyC emptyA templateLocations (\_ -> True):: SpliceConfig IO
  heist <- Heist.initHeist (HeistConfig spliceConfig "" True)
  case heist of
    Right heist' -> do
      rendered <- I.renderTemplate (hsBinding heist') $ convertString fileName
      case (rendered) of
        Just (builder, _) -> do
          return $ renderMustacheTemplate (convertString $ toLazyByteString builder) blazeBinding
        Nothing -> error "heist error"
    Left a -> error . convertString $ show a
