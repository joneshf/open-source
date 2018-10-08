{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Rollbar.Item.Request.Test where

import Data.Aeson           (Value(Object), decode', encode, toJSON)
import Data.CaseInsensitive (original)
import Data.Foldable        (fold)
import Data.Functor         (void)
import Data.HashSet         (HashSet)
import Data.Text            (Text)

import Prelude hiding (error)

import Rollbar.Item.Request (MissingHeaders(..))
import Rollbar.QuickCheck   ()

import Test.QuickCheck (conjoin, quickCheck)

import qualified Data.Text.Encoding as TE

import qualified Data.HashSet

props :: IO ()
props =
    quickCheck $ conjoin
        [ prop_valueHeadersArentWrapped
        , prop_encodingHeadersArentWrapped
        ]

prop_valueHeadersArentWrapped :: MissingHeaders '["Authorization"] -> Bool
prop_valueHeadersArentWrapped hs@(MissingHeaders rhs) =
    actual `isSubsetOf` expected
    where
    actual = keys (toJSON hs)
    expected = Data.HashSet.fromList $ fold . TE.decodeUtf8' . original . fst <$> rhs
    isSubsetOf x y = Data.HashSet.difference x y == mempty

prop_encodingHeadersArentWrapped :: MissingHeaders '["Authorization"] -> Bool
prop_encodingHeadersArentWrapped hs@(MissingHeaders rhs) =
    actual `isSubsetOf` expected
    where
    actual = foldMap keys (decode' $ encode hs)
    expected = Data.HashSet.fromList $ fold . TE.decodeUtf8' . original . fst <$> rhs
    isSubsetOf x y = Data.HashSet.difference x y == mempty

keys :: Value -> HashSet Text
keys = \case
  Object o -> Data.HashSet.fromMap (void o)
  _ -> mempty

