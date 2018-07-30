{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Rollbar.Item.MissingHeaders.Test where

import Data.Aeson (Value(Object), decode', encode, toJSON)
import Data.Text  (Text)

import Prelude hiding (error)

import Rollbar.Item.MissingHeaders (MissingHeaders(..))
import Rollbar.QuickCheck          ()

import Test.QuickCheck (conjoin, quickCheck)

import Data.Set as S

import qualified Data.HashMap.Strict

props :: IO ()
props = do
    quickCheck $ conjoin
        [ prop_valueAuthorizationIsRemoved
        , prop_encodingAuthorizationIsRemoved
        ]

    quickCheck $ conjoin
        [ prop_valueX_AccessTokenIsRemoved
        , prop_encodingX_AccessTokenIsRemoved
        ]

    quickCheck $ conjoin
        [ prop_valueAllHeadersAreRemoved
        , prop_encodingAllHeadersAreRemoved
        ]

prop_valueAuthorizationIsRemoved :: MissingHeaders '["Authorization"] -> Bool
prop_valueAuthorizationIsRemoved hs =
    "Authorization" `S.notMember` actual
    where
    actual = S.fromList (keys $ toJSON hs)

prop_encodingAuthorizationIsRemoved :: MissingHeaders '["Authorization"] -> Bool
prop_encodingAuthorizationIsRemoved hs =
    "Authorization" `S.notMember` actual
    where
    actual = S.fromList (foldMap keys $ decode' $ encode hs)

prop_valueX_AccessTokenIsRemoved :: MissingHeaders '["X-AccessToken"] -> Bool
prop_valueX_AccessTokenIsRemoved hs =
    "X-AccessToken" `S.notMember` actual
    where
    actual = S.fromList (keys $ toJSON hs)

prop_encodingX_AccessTokenIsRemoved :: MissingHeaders '["X-AccessToken"] -> Bool
prop_encodingX_AccessTokenIsRemoved hs =
    "X-AccessToken" `S.notMember` actual
    where
    actual = S.fromList (foldMap keys $ decode' $ encode hs)

prop_valueAllHeadersAreRemoved
    :: MissingHeaders
        '["Authorization", "this is made up", "Server", "X-AccessToken"]
    -> Bool
prop_valueAllHeadersAreRemoved hs =
    "Authorization" `S.notMember` actual
        && "this is made up" `S.notMember` actual
        && "Server" `S.notMember` actual
        && "X-AccessToken" `S.notMember` actual
    where
    actual = S.fromList (keys $ toJSON hs)

prop_encodingAllHeadersAreRemoved
    :: MissingHeaders
        '["Authorization", "this is made up", "Server", "X-AccessToken"]
    -> Bool
prop_encodingAllHeadersAreRemoved hs =
    "Authorization" `S.notMember` actual
        && "this is made up" `S.notMember` actual
        && "Server" `S.notMember` actual
        && "X-AccessToken" `S.notMember` actual
    where
    actual = S.fromList (foldMap keys $ decode' $ encode hs)

keys :: Value -> [Text]
keys = \case
  Object o -> Data.HashMap.Strict.keys o
  _ -> mempty
