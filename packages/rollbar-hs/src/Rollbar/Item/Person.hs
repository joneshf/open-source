{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
    Module      : Rollbar.Item.Person
    Description : The user this item affects.
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental
-}

module Rollbar.Item.Person
    ( Person(..)
    , Id(..)
    , Username(..)
    , Email(..)
    ) where

import Data.Aeson
    ( FromJSON
    , ToJSON
    , defaultOptions
    , genericToEncoding
    , genericToJSON
    , toEncoding
    , toJSON
    )
import Data.Aeson.Types (omitNothingFields)
import Data.String      (IsString)

import GHC.Generics (Generic)

import qualified Data.Text as T

-- | The affected user.
--
--  The 'Email' and 'Username' associated with the latest 'Id'
--  will overwrite any previous.
data Person
    = Person
        { id       :: Id
        , username :: Maybe Username
        , email    :: Maybe Email
        }
    deriving (Eq, Generic, Show)

instance FromJSON Person
instance ToJSON Person where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
    toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

-- | The user's identifier. This uniquely identifies a 'Person' to Rollbar.
newtype Id
    = Id T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)

-- | The user's name.
newtype Username
    = Username T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)

-- | The user's email.
newtype Email
    = Email T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)
