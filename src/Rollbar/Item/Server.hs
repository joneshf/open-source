{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
    Module      : Rollbar.Item.Server
    Description : Metadata about the server this package is running on.
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental
-}

module Rollbar.Item.Server
    ( Server(..)
    , Root(..)
    , Branch(..)
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
import Data.Maybe       (catMaybes)
import Data.String      (IsString)

import GHC.Generics (Generic)

import Network.HostName (HostName)

import Rollbar.Item.CodeVersion (CodeVersion)

import qualified Data.Text as T

-- | Information about the server using this package.
data Server
    = Server
        { host              :: Maybe HostName
        -- ^ The hostname of the server.
        , root              :: Maybe Root
        -- ^ The root directory the server is running in.
        , branch            :: Maybe Branch
        -- ^ The checked out branch the server is running on.
        , serverCodeVersion :: Maybe CodeVersion
        -- ^ The version of the server.
        }
    deriving (Eq, Generic, Show)

serverKVs :: KeyValue kv => Server -> [Maybe kv]
serverKVs Server{..} =
    [ ("host" .=) <$> host
    , ("root" .=) <$> root
    , ("branch" .=) <$> branch
    , ("code_version" .=) <$> serverCodeVersion
    ]

instance FromJSON Server where
    parseJSON (Object o) =
        Server
            <$> o .: "host"
            <*> o .: "root"
            <*> o .: "branch"
            <*> o .: "code_version"
    parseJSON v = typeMismatch "Server" v

instance ToJSON Server where
    toJSON = object . catMaybes . serverKVs
    toEncoding = pairs . mconcat . catMaybes . serverKVs

-- | The root directory.
newtype Root
    = Root T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)

-- | The git branch the server is running on.
newtype Branch
    = Branch T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)
