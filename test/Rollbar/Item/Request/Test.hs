{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Rollbar.Item.Request.Test where

import Data.Aeson           (Value(Object), decode', encode, toJSON)
import Data.CaseInsensitive (original)
import Data.Text            (Text)

import Prelude hiding (error)

import Rollbar.Item.Request (MissingHeaders(..))
import Rollbar.QuickCheck   ()

import Test.QuickCheck (conjoin, quickCheck)

import Data.Set           as S
import Data.Text.Encoding as TE

import qualified Data.HashMap.Strict

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
    actual = S.fromList (keys $ toJSON hs)
    expected = S.fromList $ either (const "") id . TE.decodeUtf8' . original . fst <$> rhs

prop_encodingHeadersArentWrapped :: MissingHeaders '["Authorization"] -> Bool
prop_encodingHeadersArentWrapped hs@(MissingHeaders rhs) =
    actual `S.isSubsetOf` expected
    where
    actual = S.fromList (foldMap keys $ decode' $ encode hs)
    expected = S.fromList $ either (const "") id . TE.decodeUtf8' . original . fst <$> rhs

keys :: Value -> [Text]
keys = \case
  Object o -> Data.HashMap.Strict.keys o
  _ -> mempty
