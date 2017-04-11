{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens ((^@..))

import Data.Aeson (toJSON)
import Data.Aeson.Lens (key, members)
import Data.Text (Text, pack)

import Prelude hiding (error)

import Rollbar.Item

import Test.QuickCheck (Arbitrary, arbitrary, elements, quickCheck)

main :: IO ()
main =
    quickCheck prop_encodedDataBodyHasRequiredKey

prop_encodedDataBodyHasRequiredKey :: Data () -> Bool
prop_encodedDataBodyHasRequiredKey x =
    length ms == 1 && fst (head ms) `elem` requiredBodyKeys
    where
    ms = toJSON x ^@.. key "body" . members

instance Arbitrary a => Arbitrary (Data a) where
    arbitrary = do
        env <- Environment . pack <$> arbitrary
        message <- fmap (MessageBody . pack) <$> arbitrary
        payload <- arbitrary
        elements $ (\f -> f env message payload) <$> datas

datas :: [Environment -> Maybe MessageBody -> a -> Data a]
datas = [debug, info, warning, error, critical]

requiredBodyKeys :: [Text]
requiredBodyKeys = ["trace", "trace_chain", "message", "crash_report"]
