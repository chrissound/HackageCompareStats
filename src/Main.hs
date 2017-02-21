{-# LANGUAGE OverloadedStrings #-}
module Main  where

import           Prelude
import           CompareForm
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get (literal "/comparePackage") comparePackageHandler
    get "/comparePackage" comparePackageFormHandler
    get "" comparePackageFormHandler
