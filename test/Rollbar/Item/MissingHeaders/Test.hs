{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Rollbar.Item.MissingHeaders.Test where

import           Data.Aeson                  (Value (Object), decode', encode,
                                              toJSON)
import           Data.Functor                (void)
import           Data.HashSet                (HashSet)
import           Data.Text                   (Text)

import           Prelude                     hiding (error)

import           Rollbar.Item.MissingHeaders (MissingHeaders (..))
import           Rollbar.QuickCheck          ()

import           Test.QuickCheck             (conjoin, quickCheck)

import qualified Data.HashSet

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
    "Authorization" `notMember` actual
    where
    actual = keys (toJSON hs)

prop_encodingAuthorizationIsRemoved :: MissingHeaders '["Authorization"] -> Bool
prop_encodingAuthorizationIsRemoved hs =
    "Authorization" `notMember` actual
    where
    actual = foldMap keys (decode' $ encode hs)

prop_valueX_AccessTokenIsRemoved :: MissingHeaders '["X-AccessToken"] -> Bool
prop_valueX_AccessTokenIsRemoved hs =
    "X-AccessToken" `notMember` actual
    where
    actual = keys (toJSON hs)

prop_encodingX_AccessTokenIsRemoved :: MissingHeaders '["X-AccessToken"] -> Bool
prop_encodingX_AccessTokenIsRemoved hs =
    "X-AccessToken" `notMember` actual
    where
    actual = foldMap keys (decode' $ encode hs)

prop_valueAllHeadersAreRemoved
    :: MissingHeaders
        '["Authorization", "this is made up", "Server", "X-AccessToken"]
    -> Bool
prop_valueAllHeadersAreRemoved hs =
    "Authorization" `notMember` actual
        && "this is made up" `notMember` actual
        && "Server" `notMember` actual
        && "X-AccessToken" `notMember` actual
    where
    actual = keys (toJSON hs)

prop_encodingAllHeadersAreRemoved
    :: MissingHeaders
        '["Authorization", "this is made up", "Server", "X-AccessToken"]
    -> Bool
prop_encodingAllHeadersAreRemoved hs =
    "Authorization" `notMember` actual
        && "this is made up" `notMember` actual
        && "Server" `notMember` actual
        && "X-AccessToken" `notMember` actual
    where
    actual = foldMap keys (decode' $ encode hs)

keys :: Value -> HashSet Text
keys = \case
  Object o -> Data.HashSet.fromMap (void o)
  _ -> mempty

notMember x = not . Data.HashSet.member x
