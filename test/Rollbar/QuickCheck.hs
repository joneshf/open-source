{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Rollbar.QuickCheck where

import Data.Bifunctor       (bimap)
import Data.CaseInsensitive (mk)
import Data.Proxy           (Proxy(Proxy))

import GHC.TypeLits (KnownSymbol, symbolVal)

import Prelude hiding (error)

import Rollbar.Item

import Test.QuickCheck

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text             as T

instance Arbitrary a => Arbitrary (Item a '["Authorization"]) where
    arbitrary = Item <$> arbitrary <*> arbitrary

instance Arbitrary AccessToken where
    arbitrary = AccessToken . T.pack <$> arbitrary

instance Arbitrary a => Arbitrary (Data a '["Authorization"]) where
    arbitrary = do
        env <- Environment . T.pack <$> arbitrary
        message <- fmap (MessageBody . T.pack) <$> arbitrary
        payload <- arbitrary
        elements $ (\f -> f env message payload) <$> datas

datas :: [Environment -> Maybe MessageBody -> a -> Data a '["Authorization"]]
datas = [debug, info, warning, error, critical]

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
