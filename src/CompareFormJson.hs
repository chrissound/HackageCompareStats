{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CompareFormJson where

import Data.Aeson
import Common

instance ToJSON APCSm where
    toEncoding = genericToEncoding defaultOptions
