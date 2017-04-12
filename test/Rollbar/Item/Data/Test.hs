{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Rollbar.Item.Data.Test where

import Control.Lens ((^@..))

import Data.Aeson      (encode, toJSON)
import Data.Aeson.Lens (key, members)
import Data.Text       (Text, pack)

import Prelude hiding (error)

import Rollbar.Item

import Test.QuickCheck
    (Arbitrary, Property, arbitrary, conjoin, elements, quickCheck)

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

instance Arbitrary a => Arbitrary (Data a '["Authorization"]) where
    arbitrary = do
        env <- Environment . pack <$> arbitrary
        message <- fmap (MessageBody . pack) <$> arbitrary
        payload <- arbitrary
        elements $ (\f -> f env message payload) <$> datas

datas :: [Environment -> Maybe MessageBody -> a -> Data a '["Authorization"]]
datas = [debug, info, warning, error, critical]

requiredBodyKeys :: [Text]
requiredBodyKeys = ["trace", "trace_chain", "message", "crash_report"]
