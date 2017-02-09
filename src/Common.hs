module Common where

import qualified Arch
import Data.String.Conversions
import Data.Text (Text)
import Web.Scotty

type APS = Arch.PackageStat
type APSm = Arch.PackagesStats

multiParam :: Text -> ActionM [Text]
multiParam x = do
  v <- params
  return $ convertString . snd <$> (filter (\z -> (fst z == convertString x)) $ v)
