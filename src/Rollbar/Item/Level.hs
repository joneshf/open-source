{-# LANGUAGE DeriveGeneric #-}

{-|
    Module      : Rollbar.Item.Level
    Description : The severity of the item.
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental
-}

module Rollbar.Item.Level
    ( Level(..)
    ) where

import Data.Aeson
    ( FromJSON
    , ToJSON
    , defaultOptions
    , genericParseJSON
    , genericToEncoding
    , genericToJSON
    , parseJSON
    , toEncoding
    , toJSON
    )
import Data.Aeson.Types (Options, constructorTagModifier)
import Data.Char        (toLower)

import GHC.Generics (Generic)

-- | Corresponds to the levels Rollbar allows in order of severity.
data Level
    = Debug
    | Info
    | Warning
    | Error
    | Critical
    deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON Level where
    parseJSON = genericParseJSON options

instance ToJSON Level where
    toJSON = genericToJSON options
    toEncoding = genericToEncoding options

options :: Options
options = defaultOptions
    { constructorTagModifier = fmap toLower
    }
