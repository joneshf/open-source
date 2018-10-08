{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
    Module      : Rollbar.Item.Internal.Notifier
    Description : Metadata about this actual package
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    Mostly, you shouldn't have to worry about this.
-}

module Rollbar.Item.Internal.Notifier
    ( Notifier(..)
    ) where

import Data.Aeson
    ( FromJSON
    , ToJSON
    , defaultOptions
    , genericToEncoding
    , toEncoding
    )
import Data.Version (Version)

import GHC.Generics (Generic)

import Rollbar.Item.Hardcoded (Hardcoded)

-- | Metadata describing this package.
data Notifier
    = Notifier
        { name    :: Hardcoded "rollbar-hs"
        -- ^ The name of this package
        , version :: Version
        -- ^ The version of this package
        }
    deriving (Eq, Generic, Show)

instance FromJSON Notifier
instance ToJSON Notifier where
    toEncoding = genericToEncoding defaultOptions
