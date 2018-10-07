{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
    Module      : Rollbar.AccessToken
    Description : The access token for a project on Rollbar.
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental
-}

module Rollbar.AccessToken
    ( AccessToken(..)
    ) where

import Data.Aeson  (FromJSON, ToJSON)
import Data.String (IsString)

import qualified Data.Text as T

-- | Should have the scope "post_server_item".
newtype AccessToken
    = AccessToken T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)
