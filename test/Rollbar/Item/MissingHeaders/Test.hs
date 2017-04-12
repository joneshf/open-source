{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Rollbar.Item.MissingHeaders.Test where

import Control.Lens ((&), (^@..))

import Data.Aeson           (encode, toJSON)
import Data.Aeson.Lens      (key, members)
import Data.Bifunctor       (bimap)
import Data.CaseInsensitive (mk, original)
import Data.Proxy           (Proxy(Proxy))
import Data.Text            (Text, pack)

import GHC.TypeLits (KnownSymbol, symbolVal)

import Prelude hiding (error)

import Rollbar.Item.MissingHeaders (MissingHeaders(..))

import Test.QuickCheck
    (Arbitrary, Gen, Property, arbitrary, conjoin, elements, quickCheck)

import Data.ByteString.Char8 as BSC8
import Data.Set              as S
import Data.Text.Encoding    as TE

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
prop_valueAuthorizationIsRemoved hs@(MissingHeaders rhs) =
    "Authorization" `S.notMember` actual
    where
    actual = toJSON hs ^@.. members & fmap fst & S.fromList

prop_encodingAuthorizationIsRemoved :: MissingHeaders '["Authorization"] -> Bool
prop_encodingAuthorizationIsRemoved hs@(MissingHeaders rhs) =
    "Authorization" `S.notMember` actual
    where
    actual = encode hs ^@.. members & fmap fst & S.fromList

prop_valueX_AccessTokenIsRemoved :: MissingHeaders '["X-AccessToken"] -> Bool
prop_valueX_AccessTokenIsRemoved hs@(MissingHeaders rhs) =
    "X-AccessToken" `S.notMember` actual
    where
    actual = toJSON hs ^@.. members & fmap fst & S.fromList

prop_encodingX_AccessTokenIsRemoved :: MissingHeaders '["X-AccessToken"] -> Bool
prop_encodingX_AccessTokenIsRemoved hs@(MissingHeaders rhs) =
    "X-AccessToken" `S.notMember` actual
    where
    actual = encode hs ^@.. members & fmap fst & S.fromList

prop_valueAllHeadersAreRemoved
    :: MissingHeaders
        '["Authorization", "this is made up", "Server", "X-AccessToken"]
    -> Bool
prop_valueAllHeadersAreRemoved hs@(MissingHeaders rhs) =
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
prop_encodingAllHeadersAreRemoved hs@(MissingHeaders rhs) =
    "Authorization" `S.notMember` actual
        && "this is made up" `S.notMember` actual
        && "Server" `S.notMember` actual
        && "X-AccessToken" `S.notMember` actual
    where
    actual = encode hs ^@.. members & fmap fst & S.fromList

instance Arbitrary (MissingHeaders '[]) where
    arbitrary = do
        xs <- arbitrary
        let hs = bimap (mk . BSC8.pack) BSC8.pack <$> xs
        pure . MissingHeaders $ hs

instance (KnownSymbol header, Arbitrary (MissingHeaders headers))
    => Arbitrary (MissingHeaders (header ': headers)) where
    arbitrary = do
        MissingHeaders hs <- arbitrary :: Gen (MissingHeaders headers)
        let name = mk . BSC8.pack $ symbolVal (Proxy :: Proxy header)
        value <- BSC8.pack <$> arbitrary
        pure . MissingHeaders $ (name, value) : hs
