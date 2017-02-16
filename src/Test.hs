{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test where

import Test.HUnit
import Main
import qualified Arch
import Data.Text (Text)

testGetComparePackage :: IO Text
testGetComparePackage = do
  statisticsStore' <- Arch.getPackagesStats "packageStatistics.json"
  case (statisticsStore') of
    Just statisticsStore -> do
      case (comparePackageGetPackages ["vim", "emacs"] statisticsStore) of
        Right aps -> do
          text <- getComparePackageTmpl ["vim", "emacs"] aps statisticsStore
          return text
        Left e -> error e
    Nothing -> error "hmm"


bobby :: IO ()
bobby = do
  texty <- testGetComparePackage
  assertEqual "grr" texty texty

test1 :: Test
test1 = do
  TestCase $ assert bobby

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = runTestTT tests
