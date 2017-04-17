{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Rollbar.Item.MissingHeaders.Test where

import Control.Lens ((&), (^@..))

import Data.Aeson      (encode, toJSON)
import Data.Aeson.Lens (members)

import Prelude hiding (error)

import Rollbar.Item.MissingHeaders (MissingHeaders(..))
import Rollbar.QuickCheck          ()

import Test.QuickCheck (conjoin, quickCheck)

import Data.Set as S

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
    actual = toJSON hs ^@.. members & fmap fst & S.fromList

prop_encodingAuthorizationIsRemoved :: MissingHeaders '["Authorization"] -> Bool
prop_encodingAuthorizationIsRemoved hs =
    "Authorization" `S.notMember` actual
    where
    actual = encode hs ^@.. members & fmap fst & S.fromList

prop_valueX_AccessTokenIsRemoved :: MissingHeaders '["X-AccessToken"] -> Bool
prop_valueX_AccessTokenIsRemoved hs =
    "X-AccessToken" `S.notMember` actual
    where
    actual = toJSON hs ^@.. members & fmap fst & S.fromList

prop_encodingX_AccessTokenIsRemoved :: MissingHeaders '["X-AccessToken"] -> Bool
prop_encodingX_AccessTokenIsRemoved hs =
    "X-AccessToken" `S.notMember` actual
    where
    actual = encode hs ^@.. members & fmap fst & S.fromList

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
    actual = toJSON hs ^@.. members & fmap fst & S.fromList

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
    actual = encode hs ^@.. members & fmap fst & S.fromList
