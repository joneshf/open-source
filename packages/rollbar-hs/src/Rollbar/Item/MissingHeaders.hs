{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
    Module      : Rollbar.Item.MissingHeaders
    Description : Headers with some missing from the JSON instance.
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental
-}

module Rollbar.Item.MissingHeaders
    ( MissingHeaders(..)
    , RemoveHeaders
    ) where

import Data.Aeson
    ( FromJSON
    , KeyValue
    , ToJSON
    , object
    , parseJSON
    , toJSON
    , (.=)
    )
import Data.Bifunctor       (bimap)
import Data.CaseInsensitive (mk, original)
import Data.Maybe           (catMaybes)
import Data.Proxy           (Proxy(Proxy))

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Network.HTTP.Types (Header, RequestHeaders)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

-- | The request headers with some missing
--
--  This is useful for removing sensitive information
--  like the `Authorization` header.
newtype MissingHeaders (headers :: [Symbol])
    = MissingHeaders RequestHeaders
    deriving (Eq, Show)

-- | Remove the headers given from the underlying request headers.
class RemoveHeaders (headers :: [Symbol]) where
    removeHeaders :: MissingHeaders headers -> RequestHeaders

instance RemoveHeaders '[] where
    removeHeaders (MissingHeaders rhs) = rhs

instance (KnownSymbol header, RemoveHeaders headers)
    => RemoveHeaders (header ': headers) where
    removeHeaders (MissingHeaders rhs) =
        removeHeaders (MissingHeaders $ filter go rhs :: MissingHeaders headers)
        where
        go (rh, _) =
            rh /= (mk . BSC8.pack $ symbolVal (Proxy :: Proxy header))

instance FromJSON (MissingHeaders headers) where
    parseJSON v = MissingHeaders . fmap (bimap (mk . BS.pack) BS.pack) <$> parseJSON v

instance RemoveHeaders headers => ToJSON (MissingHeaders headers) where
    toJSON = object . catMaybes . requestHeadersKVs . removeHeaders

requestHeadersKVs :: forall kv. KeyValue kv => RequestHeaders -> [Maybe kv]
requestHeadersKVs = fmap go
    where
    go :: Header -> Maybe kv
    go (key', val') = do
        key <- myDecodeUtf8 $ original key'
        val <- myDecodeUtf8 val'
        pure (key .= val)

myDecodeUtf8 :: BS.ByteString -> Maybe T.Text
myDecodeUtf8 = either (const Nothing) Just . TE.decodeUtf8'
