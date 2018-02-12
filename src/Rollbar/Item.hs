{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
    Module      : Rollbar.Item
    Description : Datatype for reporting to Rollbar
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    Provides a data type that subsumes most of what Rollbar expects for "item"s.

    See Rollbar's <https://rollbar.com/docs/api/items_post/#data-format Data Format>
    for more details.
-}

module Rollbar.Item
    ( -- * Data helpers
      -- |  These functions are probably what you want to use most of the time.
      --
      --    They create a 'Data' with lots of data prefilled.
      --    You can then override what you need with record updates.
      debug
    , info
    , warning
    , error
    , critical
    -- * Item
    , Item(..)
    -- * Item data
    , module Rollbar.AccessToken
    , AccessToken(..)

    , module Rollbar.Item.Data
    , Data(..)
    , Context(..)
    , Fingerprint(..)
    , Framework(..)
    , Title(..)
    , UUID4(..)

    -- * Required data
    , module Rollbar.Item.Body
    , Body(..)
    , MessageBody(..)

    , module Rollbar.Item.Environment
    , Environment(..)

    , module Rollbar.Item.Level
    , Level(..)

    -- * Optional data
    , module Rollbar.Item.CodeVersion
    , CodeVersion(..)

    , module Rollbar.Item.Hardcoded
    , Hardcoded(..)

    , module Rollbar.Item.MissingHeaders
    , MissingHeaders(..)

    , module Rollbar.Item.Person
    , Person(..)
    , Email(..)
    , Id(..)
    , Username(..)

    , module Rollbar.Item.Request
    , Get(..)
    , IP(..)
    , Method(..)
    , MissingHeaders(..)
    , QueryString(..)
    , RawBody(..)
    , URL(..)

    , module Rollbar.Item.Server
    , Server(..)
    , Branch(..)
    , Root(..)
    ) where

import Data.Aeson
    ( FromJSON
    , KeyValue
    , ToJSON
    , Value(Object)
    , object
    , pairs
    , parseJSON
    , toEncoding
    , toJSON
    , (.:)
    , (.=)
    )
import Data.Aeson.Types (typeMismatch)
import Data.Maybe       (fromMaybe)

import GHC.Generics (Generic)

import Prelude hiding (error)

import Rollbar.AccessToken
import Rollbar.Item.Body
import Rollbar.Item.CodeVersion
import Rollbar.Item.Data
import Rollbar.Item.Environment
import Rollbar.Item.Hardcoded
import Rollbar.Item.Level
import Rollbar.Item.MissingHeaders
import Rollbar.Item.Person
import Rollbar.Item.Request
import Rollbar.Item.Server

import Rollbar.Item.Internal.Notifier
import Rollbar.Item.Internal.Platform

import System.Info (os)

import qualified Data.Text                    as T
import qualified Paths_wai_middleware_rollbar

-- | Creates 'Data' with the level set to 'Debug'.
debug
    :: Environment
    -> Maybe MessageBody
    -> payload
    -> Data payload ("Authorization" ': headers)
debug environment messageBody payload =
    Data
        { body = Message (fromMaybe "" messageBody) payload
        , codeVersion = Nothing
        , context = Nothing
        , custom = Nothing
        , environment = environment
        , fingerprint = Nothing
        , framework = Nothing
        , language = Hardcoded
        , level = Debug
        , notifier = Notifier Hardcoded Paths_wai_middleware_rollbar.version
        , person = Nothing
        , platform = Platform $ T.pack os
        , request = Nothing
        , server = Nothing
        , timestamp = Nothing
        , title = Nothing
        , uuid = Nothing
        }

-- | Creates 'Data' with the level set to 'Info'.
info
    :: Environment
    -> Maybe MessageBody
    -> payload
    -> Data payload ("Authorization" ': headers)
info environment messageBody payload =
    (debug environment messageBody payload) { level = Info }

-- | Creates 'Data' with the level set to 'Warning'.
warning
    :: Environment
    -> Maybe MessageBody
    -> payload
    -> Data payload ("Authorization" ': headers)
warning environment messageBody payload =
    (debug environment messageBody payload) { level = Warning }

-- | Creates 'Data' with the level set to 'Error'.
error
    :: Environment
    -> Maybe MessageBody
    -> payload
    -> Data payload ("Authorization" ': headers)
error environment messageBody payload =
    (debug environment messageBody payload) { level = Error }

-- | Creates 'Data' with the level set to 'Critical'.
critical
    :: Environment
    -> Maybe MessageBody
    -> payload
    -> Data payload ("Authorization" ': headers)
critical environment messageBody payload =
    (debug environment messageBody payload) { level = Critical }

-- | The thing we actually give to Rollbar.
data Item a headers
    = Item
        { accessToken :: AccessToken
        -- ^ Should have a scope "post_server_item".
        , itemData    :: Data a headers
        }
    deriving (Eq, Generic, Show)

itemKVs
    :: (KeyValue kv, RemoveHeaders headers, ToJSON v)
    => Item v headers
    -> [kv]
itemKVs Item{accessToken, itemData} =
    [ "access_token" .= accessToken
    , "data" .= itemData
    ]

instance FromJSON a => FromJSON (Item a headers) where
    parseJSON (Object o) = Item <$> o .: "access_token" <*> o .: "data"
    parseJSON v          = typeMismatch "Item a headers" v

instance (RemoveHeaders headers, ToJSON a) => ToJSON (Item a headers) where
    toJSON = object . itemKVs
    toEncoding = pairs . mconcat . itemKVs
