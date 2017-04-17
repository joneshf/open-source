{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
    Module      : Rollbar.Item.CodeVersion
    Description : Metadata for describing versions of software
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental
-}

module Rollbar.Item.CodeVersion
    ( CodeVersion(..)
    ) where

import Data.Aeson       (FromJSON, ToJSON, parseJSON, toEncoding, toJSON)
import Data.Aeson.Types (typeMismatch)

import GHC.Generics (Generic)

import qualified Data.Aeson as A
import qualified Data.Text  as T

-- | Rollbar supports different ways to say what version the code is.
data CodeVersion
    -- | Good ole SemVer.
    --  It's 'T.Text' because who knows if you actually got it right...
    = SemVer T.Text
    -- | Plain integers.
    | Number Int
    -- | Should be a Git SHA.
    | SHA T.Text
    deriving (Eq, Generic, Show)

prettyCodeVersion :: CodeVersion -> T.Text
prettyCodeVersion (SemVer s) = s
prettyCodeVersion (Number n) = T.pack . show $ n
prettyCodeVersion (SHA h)    = h

instance FromJSON CodeVersion where
    parseJSON (A.String s) = case T.splitOn "." s of
        [_major, _minor, _patch] -> pure $ SemVer s
        _                        -> pure $ SHA s
    parseJSON (A.Number n) = pure $ Number $ floor n
    parseJSON v = typeMismatch "CodeVersion" v

instance ToJSON CodeVersion where
    toJSON = toJSON . prettyCodeVersion
    toEncoding = toEncoding . prettyCodeVersion
