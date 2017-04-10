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
    ( ToJSON
    , defaultOptions
    , genericToEncoding
    , genericToJSON
    , toEncoding
    , toJSON
    )
import Data.Aeson.Types (constructorTagModifier)
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

instance ToJSON Level where
    toJSON = genericToJSON defaultOptions
        { constructorTagModifier = fmap toLower
        }
    toEncoding = genericToEncoding defaultOptions
        { constructorTagModifier = fmap toLower
        }
