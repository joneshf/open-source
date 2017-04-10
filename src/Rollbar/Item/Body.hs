{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import Data.Aeson  (KeyValue, ToJSON, object, pairs, toEncoding, toJSON, (.=))
import Data.String (IsString)

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
bodyKVs Message{..} =
    [ "body" .= messageBody
    , "data" .= messageData
    ]

instance ToJSON arbitrary => ToJSON (Body arbitrary) where
    toJSON = object . bodyKVs
    toEncoding = pairs . mconcat . bodyKVs

-- | The primary message text to send to Rollbar.
newtype MessageBody
    = MessageBody T.Text
    deriving (Eq, IsString, Show, ToJSON)
