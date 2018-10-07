{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , IP(..)
    , Method(..)
    , MissingHeaders(..)
    , QueryString(..)
    , RawBody(..)
    , URL(..)
    , RemoveHeaders
    ) where

import Data.Aeson
    ( FromJSON
    , KeyValue
    , ToJSON
    , Value(Object, String)
    , object
    , pairs
    , parseJSON
    , toEncoding
    , toJSON
    , (.:)
    , (.=)
    )
import Data.Aeson.Types (typeMismatch)
import Data.Bifunctor   (bimap)
import Data.Maybe       (catMaybes, fromMaybe)
import Data.String      (IsString)

import GHC.Generics (Generic)

import Network.HTTP.Types (Query)
import Network.Socket     (SockAddr(SockAddrInet), tupleToHostAddress)

import Rollbar.Item.MissingHeaders

import Text.Read (readMaybe)

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

-- | Data sent to the server
data Request headers
    = Request
        { rawBody     :: RawBody
        , get         :: Get
        -- ^ Query parameters
        , headers     :: MissingHeaders headers
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

instance FromJSON RawBody where
    parseJSON v = RawBody . BS.pack <$> parseJSON v

instance ToJSON RawBody where
    toJSON (RawBody body) = toJSON (myDecodeUtf8 body)
    toEncoding (RawBody body) = toEncoding (myDecodeUtf8 body)

-- | The query string parameters as a more useful data structure.
newtype Get
    = Get Query
    deriving (Eq, Generic, Show)

instance FromJSON Get where
    parseJSON v = Get . fmap (bimap BS.pack (fmap BS.pack)) <$> parseJSON v

instance ToJSON Get where
    toJSON (Get q) = object . catMaybes . queryKVs $ q
    toEncoding (Get q) = pairs . mconcat . catMaybes . queryKVs $ q

queryKVs :: forall kv. (KeyValue kv) => Query -> [Maybe kv]
queryKVs = fmap go
    where
    go :: (BS.ByteString, Maybe BS.ByteString) -> Maybe kv
    go (key', val') = do
        key <- myDecodeUtf8 key'
        let val = val' >>= myDecodeUtf8
        pure (key .= val)

-- | The HTTP Verb
newtype Method
    = Method BS.ByteString
    deriving (Eq, Generic, Show)

instance FromJSON Method where
    parseJSON v = Method . BS.pack <$> parseJSON v

instance ToJSON Method where
    toJSON (Method q) = toJSON (myDecodeUtf8 q)
    toEncoding (Method q) = toEncoding (myDecodeUtf8 q)

-- | The raw querystring.
newtype QueryString
    = QueryString BS.ByteString
    deriving (Eq, Generic, Show)

instance FromJSON QueryString where
    parseJSON v = QueryString . BS.pack <$> parseJSON v

instance ToJSON QueryString where
    toJSON (QueryString q) = toJSON (myDecodeUtf8' q)
    toEncoding (QueryString q) = toEncoding (myDecodeUtf8' q)

-- | The IP address of the client.
newtype IP
    = IP SockAddr
    deriving (Eq, Generic, Show)

instance FromJSON IP where
    parseJSON v@(String s) = case T.splitOn "." s of
        [a', b', c', d] -> case T.splitOn ":" d of
            [e', f'] -> maybe (typeMismatch "IP" v) pure $ do
                [a, b, c, e] <- traverse (readMaybe . T.unpack) [a', b', c', e']
                f <- (readMaybe . T.unpack) f'
                pure . IP . SockAddrInet f $ tupleToHostAddress (a, b, c, e)
            _ -> typeMismatch "IP" v
        _ -> typeMismatch "IP" v
    parseJSON v = typeMismatch "IP" v

instance ToJSON IP where
    toJSON (IP ip) = toJSON (show ip)
    toEncoding (IP ip) = toEncoding (show ip)

requestKVs :: (KeyValue kv, RemoveHeaders headers) => Request headers -> [kv]
requestKVs Request{get, headers, method, queryString, rawBody, url, userIP} =
    [ "body" .= rawBody
    , "GET" .= get
    , "headers" .= headers
    , "method" .= method
    , "query_string" .= queryString
    , "url" .= url
    , "user_ip" .= userIP
    ]

instance FromJSON (Request headers) where
    parseJSON (Object o) =
        Request
            <$> o .: "body"
            <*> o .: "GET"
            <*> o .: "headers"
            <*> o .: "method"
            <*> o .: "query_string"
            <*> o .: "url"
            <*> o .: "user_ip"
    parseJSON v = typeMismatch "Request headers" v

instance (RemoveHeaders headers) => ToJSON (Request headers) where
    toJSON = object . requestKVs
    toEncoding = pairs . mconcat . requestKVs

-- | The URL as a slightly more useful structure.
newtype URL
    = URL (Maybe BS.ByteString, [T.Text])
    deriving (Eq, Generic, Show)

prettyURL :: URL -> T.Text
prettyURL (URL (host, parts)) =
    T.intercalate "/" (fromMaybe "" (host >>= myDecodeUtf8) : parts)

instance FromJSON URL where
    parseJSON (String s) = case T.splitOn "/" s of
        host:parts | "http" `T.isPrefixOf` host -> pure $ URL (Just $ TE.encodeUtf8 host, parts)
        parts -> pure $ URL (Nothing, parts)
    parseJSON v       = typeMismatch "URL" v

instance ToJSON URL where
    toJSON = toJSON . prettyURL
    toEncoding = toEncoding . prettyURL

myDecodeUtf8 :: BS.ByteString -> Maybe T.Text
myDecodeUtf8 = either (const Nothing) Just . TE.decodeUtf8'

myDecodeUtf8' :: BS.ByteString -> T.Text
myDecodeUtf8' = fromMaybe "" . myDecodeUtf8
