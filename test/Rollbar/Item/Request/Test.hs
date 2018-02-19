{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Rollbar.Item.Request.Test where

import Control.Lens ((&), (^@..))

import Data.Aeson           (encode, toJSON)
import Data.Aeson.Lens      (members)
import Data.CaseInsensitive (original)

import Prelude hiding (error)

import Rollbar.Item.Request (MissingHeaders(..))
import Rollbar.QuickCheck   ()

import Test.QuickCheck (conjoin, quickCheck)

import Data.Set           as S
import Data.Text.Encoding as TE

props :: IO ()
props =
    quickCheck $ conjoin
        [ prop_valueHeadersArentWrapped
        , prop_encodingHeadersArentWrapped
        ]

prop_valueHeadersArentWrapped :: MissingHeaders '["Authorization"] -> Bool
prop_valueHeadersArentWrapped hs@(MissingHeaders rhs) =
    actual `S.isSubsetOf` expected
    where
    actual = toJSON hs ^@.. members & fmap fst & S.fromList
    expected = S.fromList $ either (const "") id . TE.decodeUtf8' . original . fst <$> rhs

prop_encodingHeadersArentWrapped :: MissingHeaders '["Authorization"] -> Bool
prop_encodingHeadersArentWrapped hs@(MissingHeaders rhs) =
    actual `S.isSubsetOf` expected
    where
    actual = encode hs ^@.. members & fmap fst & S.fromList
    expected = S.fromList $ either (const "") id . TE.decodeUtf8' . original . fst <$> rhs
