{-# LANGUAGE DeriveGeneric #-}

{-|
    Module      : Network.Wai.Middleware.Rollbar.Payload
    Description : The payload to send to Rollbar
    Copyright   : (c) Hardy Jones, 2018
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    Provides the payload for communicating with Rollbar.
-}

module Network.Wai.Middleware.Rollbar.Payload where

import Data.Aeson (ToJSON, defaultOptions, genericToEncoding, toEncoding)

import GHC.Generics (Generic)

import qualified Data.Text as T

data Payload
    = RequestPayload
        { statusCode    :: Int
        , statusMessage :: T.Text
        , userAgent     :: Maybe T.Text
        , range         :: Maybe T.Text
        , referer       :: Maybe T.Text
        }
    | ExceptionPayload
        { exception :: T.Text
        , userAgent :: Maybe T.Text
        , range     :: Maybe T.Text
        , referer   :: Maybe T.Text
        }
    deriving (Generic, Show)

instance ToJSON Payload where
    toEncoding = genericToEncoding defaultOptions
