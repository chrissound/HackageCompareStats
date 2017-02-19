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

type TemplateName = String
type HeistBind = HeistState IO -> HeistState IO

renderMustacheTemplate :: String -> Value -> Maybe Text
renderMustacheTemplate name binding = case (compileTemplate "" (convertString name)) of
  Right template -> Just $ substituteValue template binding
  Left _ -> Nothing

renderHeistTemplatePath :: TemplateName -> HeistBind -> IO (Either Text Text)
renderHeistTemplatePath fileName hsBinding = do
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
        Just (builder, _) -> return . return . convertString $ toLazyByteString builder
        Nothing -> return . Left $ "Heist render error. No further information"
    Left a -> return . Left . convertString . show $ a

renderTemplate :: TemplateName -> HeistBind -> Maybe Value -> IO Text
renderTemplate fileName hsBinding mBinding = do
  heistRender <- renderHeistTemplatePath fileName hsBinding
  case heistRender of
    Right heistRender' -> case mBinding of
      Just mBinding' -> case renderMustacheTemplate (convertString heistRender') mBinding' of
        Just x -> return x
        Nothing -> error "Mustache render error. Failed to render "
      Nothing -> return $ convertString heistRender'
    Left e -> error $ convertString e
