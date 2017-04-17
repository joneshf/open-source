{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
    Module      : Rollbar.Item.Environment
    Description : The environment from which this package is running.
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental
-}

module Rollbar.Item.Environment
    ( Environment(..)
    ) where

import Data.Aeson  (FromJSON, ToJSON)
import Data.String (IsString)

import qualified Data.Text as T

-- | Should be something meaningful to your program.
--  E.g. "development", "production", "staging"
newtype Environment
    = Environment T.Text
    deriving (Eq, FromJSON, IsString, Show, ToJSON)
