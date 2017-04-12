{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
    Module      : Rollbar.Item.Data
    Description : Metadata about the item being reported
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    The majority of this module is metadata that is reported for each item.
-}

module Rollbar.Item.Data
    ( Data(..)
    , Context(..)
    , Fingerprint(..)
    , Framework(..)
    , Title(..)
    , UUID4(..)

    , RemoveHeaders
    ) where

import Data.Aeson
    ( ToJSON
    , Value
    , defaultOptions
    , genericToEncoding
    , genericToJSON
    , toEncoding
    , toJSON
    )
import Data.Aeson.Types (fieldLabelModifier, omitNothingFields)
import Data.String      (IsString)
import Data.Time        (UTCTime)
import Data.UUID        (UUID, toText)

import GHC.Generics (Generic)

import Rollbar.Item.Body        (Body)
import Rollbar.Item.CodeVersion (CodeVersion)
import Rollbar.Item.Environment (Environment)
import Rollbar.Item.Hardcoded   (Hardcoded)
import Rollbar.Item.Level       (Level)
import Rollbar.Item.Person      (Person)
import Rollbar.Item.Request     (RemoveHeaders, Request)
import Rollbar.Item.Server      (Server)

import Rollbar.Item.Internal.Notifier (Notifier)
import Rollbar.Item.Internal.Platform (Platform)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text         as T

-- | The main payload of an item.
--  Most of this is metadata.
--
--  N.B. While it's entirely possible for you to create one of these yourself,
--  it's usually easier to use helpers like 'info' and 'error'.
data Data body headers
    = Data
        { body        :: Body body
        , codeVersion :: CodeVersion
        -- ^ Metadata about this package. You probably shouldn't create this yourself.
        , context     :: Maybe Context
        , custom      :: Maybe (HM.HashMap T.Text Value)
        , environment :: Environment
        , fingerprint :: Maybe Fingerprint
        , framework   :: Maybe Framework
        , language    :: Hardcoded "haskell"
        , level       :: Level
        , notifier    :: Notifier
        -- ^ Metadata about this package. You probably shouldn't create this yourself.
        , person      :: Maybe Person
        , platform    :: Platform
        -- ^ Metadata about this package. You probably shouldn't create this yourself.
        , request     :: Maybe (Request headers)
        , server      :: Maybe Server
        , timestamp   :: Maybe UTCTime
        , title       :: Maybe Title
        , uuid        :: Maybe UUID4
        }
    deriving (Eq, Generic, Show)

instance (RemoveHeaders headers, ToJSON body) => ToJSON (Data body headers) where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = codeVersionModifier
        , omitNothingFields = True
        }
    toEncoding = genericToEncoding defaultOptions
        { fieldLabelModifier = codeVersionModifier
        , omitNothingFields = True
        }

codeVersionModifier :: (Eq s, IsString s) => s -> s
codeVersionModifier = \case
    "codeVersion" -> "code_version"
    str -> str

-- | The framework that is using this package.
--  E.g. "scotty", "servant", "yesod"
newtype Framework
    = Framework T.Text
    deriving (Eq, IsString, Show, ToJSON)

-- | The place in the code where this item came from.
newtype Context
    = Context T.Text
    deriving (Eq, IsString, Show, ToJSON)

-- | How to group the item.
newtype Fingerprint
    = Fingerprint T.Text
    deriving (Eq, IsString, Show, ToJSON)

-- | The title of the item.
newtype Title
    = Title T.Text
    deriving (Eq, IsString, Show, ToJSON)

-- | A unique identifier for each item.
newtype UUID4
    = UUID4 UUID
    deriving (Eq, Generic, Show)

instance ToJSON UUID4 where
    toJSON (UUID4 u) = toJSON (toText u)
    toEncoding (UUID4 u) = toEncoding (toText u)
