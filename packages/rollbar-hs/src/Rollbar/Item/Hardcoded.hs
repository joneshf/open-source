{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
    Module      : Rollbar.Item.Hardcoded
    Description : Provides a way to hard code a value in JSOn
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    Probably this could live outside the package...
-}

module Rollbar.Item.Hardcoded
    ( Hardcoded(..)
    ) where

import Data.Aeson
    (FromJSON, ToJSON, Value(String), parseJSON, toEncoding, toJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Text        (pack)

import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | This is basically 'Data.Proxy' with the variable restricted to 'Symbol'.
--  It's mostly useful so a value can be insert into a JSON blob easily.
data Hardcoded (symbol :: Symbol)
    = Hardcoded
    deriving (Eq, Generic, Show)

instance KnownSymbol symbol => ToJSON (Hardcoded symbol) where
    toJSON = toJSON . symbolVal
    toEncoding = toEncoding . symbolVal

instance KnownSymbol symbol => FromJSON (Hardcoded symbol) where
    parseJSON (String str)
        | str == pack (symbolVal (Hardcoded :: Hardcoded symbol)) = pure Hardcoded
    parseJSON v = typeMismatch "Hardcoded symbol" v
