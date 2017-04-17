{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
    Module      : Rollbar.Item.Internal.Platform
    Description : Metadata describing the platform this package is running on.
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    Mostly, you shouldn't have to worry about this.
-}

module Rollbar.Item.Internal.Platform
    ( Platform(..)
    ) where

import Data.Aeson  (FromJSON, ToJSON)
import Data.String (IsString)

import qualified Data.Text as T

-- | Should be something meaningful to rollbar, like "linux".
newtype Platform
    = Platform T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)
