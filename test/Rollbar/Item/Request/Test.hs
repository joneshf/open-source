{-# LANGUAGE OverloadedStrings #-}
module Rollbar.Item.Request.Test where

import Control.Lens ((&), (^@..))

import Data.Aeson           (encode, toJSON)
import Data.Aeson.Lens      (key, members)
import Data.Bifunctor       (bimap)
import Data.CaseInsensitive (mk, original)
import Data.Text            (Text, pack)

import Prelude hiding (error)

import Rollbar.Item.Request (Headers(..))

import Test.QuickCheck
    (Arbitrary, Property, arbitrary, conjoin, elements, quickCheck)

import Data.ByteString.Char8 as BSC8
import Data.Set              as S
import Data.Text.Encoding    as TE

props :: Property
props =
    conjoin
        [ prop_valueHeadersArentWrapped
        , prop_encodingHeadersArentWrapped
        ]

prop_valueHeadersArentWrapped :: Headers -> Bool
prop_valueHeadersArentWrapped hs@(Headers rhs) =
    actual `S.isSubsetOf` expected
    where
    actual = toJSON hs ^@.. members & fmap fst & S.fromList
    expected = S.fromList $ either (const "") id . TE.decodeUtf8' . original . fst <$> rhs

prop_encodingHeadersArentWrapped :: Headers -> Bool
prop_encodingHeadersArentWrapped hs@(Headers rhs) =
    actual `S.isSubsetOf` expected
    where
    actual = encode hs ^@.. members & fmap fst & S.fromList
    expected = S.fromList $ either (const "") id . TE.decodeUtf8' . original . fst <$> rhs

instance Arbitrary Headers where
    arbitrary = do
        xs <- arbitrary
        pure . Headers $ bimap (mk . BSC8.pack) BSC8.pack <$> xs
