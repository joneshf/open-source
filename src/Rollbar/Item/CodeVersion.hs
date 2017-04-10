{-# LANGUAGE DeriveGeneric #-}

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

import Data.Aeson (ToJSON, toEncoding, toJSON)

import GHC.Generics (Generic)

import qualified Data.Text as T

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

instance ToJSON CodeVersion where
    toJSON = toJSON . prettyCodeVersion
    toEncoding = toEncoding . prettyCodeVersion
