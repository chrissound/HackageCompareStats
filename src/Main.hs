{-# LANGUAGE OverloadedStrings #-}
module Main  where


import           Network.Wai.Middleware.RequestLogger
import           Prelude
import           CompareForm
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get (literal "/comparePackage") comparePackageHandler
    get "/comparePackage" comparePackageHandler
    get "" comparePackageHandler
