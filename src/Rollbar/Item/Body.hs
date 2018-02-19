{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
    Module      : Rollbar.Item.Body
    Description : The meat of the Rollbar item
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    Use this for providing the majority of your specific data to Rollbar.
-}

module Rollbar.Item.Body
    ( Body(..)
    , MessageBody(..)
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
import Data.Aeson.Encoding (pair)
import Data.Aeson.Types    (typeMismatch)
import Data.String         (IsString)

import GHC.Generics (Generic)

import qualified Data.Text as T

-- | This is the actual data that you want to give to Rollbar.
--  Most of the rest of Rollbar is metadata.
data Body arbitrary
    -- | No stack trace, just a message and some arbitrary data.
    = Message
        { messageBody :: MessageBody
        -- ^ The primary message text.
        , messageData :: arbitrary
        -- ^ Any arbitrary data you want to send with this message.
        }
    deriving (Eq, Generic, Show)

bodyKVs :: (KeyValue kv, ToJSON v) => Body v -> [kv]
bodyKVs Message{messageBody, messageData} =
    [ "body" .= messageBody
    , "data" .= messageData
    ]

instance FromJSON arbitrary => FromJSON (Body arbitrary) where
    parseJSON (Object o') = do
        o <- o' .: "message"
        Message <$> o .: "body" <*> o .: "data"
    parseJSON v = typeMismatch "Body arbitrary" v

instance ToJSON arbitrary => ToJSON (Body arbitrary) where
    toJSON x = object ["message" .= object (bodyKVs x)]
    toEncoding = pairs . pair "message" . pairs . mconcat . bodyKVs

-- | The primary message text to send to Rollbar.
newtype MessageBody
    = MessageBody T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)
