{-# LANGUAGE OverloadedStrings #-}
module Render where

import qualified Heist
import Data.String.Conversions
import Data.ByteString.Builder (toLazyByteString)
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as HeistCom
import Text.Mustache
import Data.Map.Syntax
import Heist.Internal.Types
import Data.Text (Text)
import Text.Mustache.Types

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
          let grr = toLazyByteString builder
          let templateYo = compileTemplate "hmm" (convertString grr) 
          case templateYo of
            (Right tempalteYolo) -> return $ substituteValue tempalteYolo blazeBinding
            (Left _) -> error "Failed to compile template"
        Nothing -> error "heist error"
    Left a -> error . convertString $ show a
