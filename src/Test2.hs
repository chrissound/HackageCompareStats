{-# LANGUAGE OverloadedStrings #-}
module Test2 where

import Data.Aeson
import Text.Mustache

main :: IO ()
main = do
  let example = Data.Aeson.object [ "key" .= (5 :: Integer), "somethingElse" .= (2 :: Integer) ]
  print . encode $ example
  print ("Start" :: String)
  case compileTemplate "" "{{{jsonData}}}" of
    Right x -> do
      print $ substituteValue x (Text.Mustache.object ["jsonData" ~= example])
    Left e -> error . show $ e
