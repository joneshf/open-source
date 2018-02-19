{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Rollbar.Item.Data.Test where

import Control.Lens ((^@..))

import Data.Aeson      (encode, toJSON)
import Data.Aeson.Lens (key, members)
import Data.Text       (Text)

import Rollbar.Item
import Rollbar.QuickCheck ()

import Test.QuickCheck (conjoin, quickCheck)

props :: IO ()
props =
    quickCheck $ conjoin
        [ prop_valueDataBodyHasRequiredKey
        , prop_encodingDataBodyHasRequiredKey
        ]

prop_valueDataBodyHasRequiredKey :: Data () '["Authorization"] -> Bool
prop_valueDataBodyHasRequiredKey x =
    length ms == 1 && fst (head ms) `elem` requiredBodyKeys
    where
    ms = toJSON x ^@.. key "body" . members

prop_encodingDataBodyHasRequiredKey :: Data () '["Authorization"] -> Bool
prop_encodingDataBodyHasRequiredKey x =
    length ms == 1 && fst (head ms) `elem` requiredBodyKeys
    where
    ms = encode x ^@.. key "body" . members

requiredBodyKeys :: [Text]
requiredBodyKeys = ["trace", "trace_chain", "message", "crash_report"]
