{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
    Module      : Rollbar.Item.Request
    Description : Data about the request sent to the server.
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental
-}

module Rollbar.Item.Request
    ( Request(..)
    , Get(..)
    , Headers(..)
    , IP(..)
    , Method(..)
    , QueryString(..)
    , RawBody(..)
    , URL(..)
    ) where

import Data.Aeson
    (KeyValue, ToJSON, Value, object, pairs, toEncoding, toJSON, (.=))
import Data.CaseInsensitive (original)
import Data.Maybe           (fromMaybe, mapMaybe)
import Data.String          (IsString)

import GHC.Generics (Generic)

import Network.HTTP.Types (Header, Query, RequestHeaders)
import Network.Socket     (SockAddr)

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

-- | Data sent to the server
data Request
    = Request
        { rawBody     :: RawBody
        , get         :: Get
        -- ^ Query parameters
        , headers     :: Headers
        , method      :: Method
        , queryString :: QueryString
        -- ^ The entire query string
        , url         :: URL
        , userIP      :: IP
        -- ^ The client's IP address
        }
    deriving (Eq, Generic, Show)

-- | The raw request body as a 'BS.ByteString'.
newtype RawBody
    = RawBody BS.ByteString
    deriving (Eq, Generic, IsString, Show)

instance ToJSON RawBody where
    toJSON (RawBody body) = toJSON (myDecodeUtf8 body)
    toEncoding (RawBody body) = toEncoding (myDecodeUtf8 body)

-- | The query string parameters as a more useful data structure.
newtype Get
    = Get Query
    deriving (Eq, Generic, Show)

instance ToJSON Get where
    toJSON (Get q) = toJSON (toJSONQuery q)
    -- TODO: Implement more efficient version
    toEncoding (Get q) = toEncoding (toJSONQuery q)

toJSONQuery :: Query -> Value
toJSONQuery = object . mapMaybe go
    where
    go :: (BS.ByteString, Maybe BS.ByteString) -> Maybe (T.Text, Value)
    go (key', val') = do
        key <- myDecodeUtf8 key'
        let val = val' >>= myDecodeUtf8
        pure (key, toJSON val)

-- | The request headers
newtype Headers
    = Headers RequestHeaders
    deriving (Eq, Generic, Show)

instance ToJSON Headers where
    toJSON (Headers hs) = toJSON (toJSONRequestHeaders hs)
    -- TODO: Implement more efficient version
    toEncoding (Headers hs) = toEncoding (toJSONRequestHeaders hs)

toJSONRequestHeaders :: RequestHeaders -> Value
toJSONRequestHeaders = object . mapMaybe go
    where
    go :: Header -> Maybe (T.Text, Value)
    go (key', val') = do
        key <- myDecodeUtf8 $ original key'
        val <- myDecodeUtf8 val'
        pure (T.pack . show $ key, toJSON val)

-- | The HTTP Verb
newtype Method
    = Method BS.ByteString
    deriving (Eq, Generic, Show)

instance ToJSON Method where
    toJSON (Method q) = toJSON (myDecodeUtf8 q)
    toEncoding (Method q) = toEncoding (myDecodeUtf8 q)

-- | The raw querystring.
newtype QueryString
    = QueryString BS.ByteString
    deriving (Eq, Generic, Show)

instance ToJSON QueryString where
    toJSON (QueryString q) = toJSON (myDecodeUtf8' q)
    toEncoding (QueryString q) = toEncoding (myDecodeUtf8' q)

-- | The IP address of the client.
newtype IP
    = IP SockAddr
    deriving (Eq, Generic, Show)

instance ToJSON IP where
    toJSON (IP ip) = toJSON (show ip)
    toEncoding (IP ip) = toEncoding (show ip)

requestKVs :: KeyValue kv => Request -> [kv]
requestKVs Request{..} =
    [ "body" .= rawBody
    , "GET" .= get
    , "headers" .= headers
    , "method" .= method
    , "query_string" .= queryString
    , "url" .= url
    , "user_ip" .= userIP
    ]

instance ToJSON Request where
    toJSON = object . requestKVs
    toEncoding = pairs . mconcat . requestKVs

-- | The URL as a slightly more useful structure.
newtype URL
    = URL (Maybe BS.ByteString, [T.Text])
    deriving (Eq, Generic, Show)

prettyURL :: URL -> T.Text
prettyURL (URL (host, parts)) =
    T.intercalate "/" (fromMaybe "" (host >>= myDecodeUtf8) : parts)

instance ToJSON URL where
    toJSON = toJSON . prettyURL
    toEncoding = toEncoding . prettyURL

myDecodeUtf8 :: BS.ByteString -> Maybe T.Text
myDecodeUtf8 = either (const Nothing) Just . TE.decodeUtf8'

myDecodeUtf8' :: BS.ByteString -> T.Text
myDecodeUtf8' = fromMaybe "" . myDecodeUtf8
