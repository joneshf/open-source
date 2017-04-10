{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Rollbar.Item where

import Data.Aeson
    ( KeyValue
    , ToJSON
    , Value
    , defaultOptions
    , genericToEncoding
    , genericToJSON
    , object
    , pairs
    , toEncoding
    , toJSON
    , (.=)
    )
import Data.Aeson.Types     (constructorTagModifier, omitNothingFields)
import Data.CaseInsensitive (original)
import Data.Char            (toLower)
import Data.Maybe           (fromMaybe, mapMaybe)
import Data.String          (IsString)
import Data.Time            (UTCTime)
import Data.UUID            (UUID, toText)
import Data.Version         (Version, showVersion)

import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Network.HostName   (HostName)
import Network.HTTP.Types (Header, Query, RequestHeaders)
import Network.Socket     (SockAddr)

import System.Info (os)

import qualified Data.ByteString              as BS
import qualified Data.HashMap.Lazy            as HM
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Paths_wai_middleware_rollbar

data Item a
    = Item
        { accessToken :: AccessToken
        , data'       :: Data a
        }
    deriving (Eq, Generic, Show)

itemKVs :: (KeyValue kv, ToJSON v) => Item v -> [kv]
itemKVs Item{..} =
    [ "access_token" .= accessToken
    , "data" .= data'
    ]

instance ToJSON a => ToJSON (Item a) where
    toJSON = object . itemKVs
    toEncoding = pairs . mconcat . itemKVs

newtype AccessToken
    = AccessToken T.Text
    deriving (Eq, IsString, Show, ToJSON)

data Data body
    = Data
        { body        :: Body body
        , codeVersion :: Maybe CodeVersion
        , context     :: Maybe Context
        , custom      :: Maybe (HM.HashMap T.Text Value)
        , environment :: Environment
        , fingerprint :: Maybe Fingerprint
        , framework   :: Maybe Framework
        , language    :: Hardcoded "haskell"
        , level       :: Maybe Level
        , notifier    :: Maybe Notifier
        , person      :: Maybe Person
        , platform    :: Maybe Platform
        , request     :: Maybe Request
        , server      :: Maybe Server
        , timestamp   :: Maybe UTCTime
        , title       :: Maybe Title
        , uuid        :: Maybe UUID4
        }
    deriving (Eq, Generic, Show)

instance ToJSON body => ToJSON (Data body) where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
    toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

error :: Maybe Body' -> data' -> Data data'
error body' data' =
    Data
        { body = Message (fromMaybe "" body') data'
        , codeVersion =
            Just . SemVer . T.pack $ showVersion Paths_wai_middleware_rollbar.version
        , context = Nothing
        , custom = Nothing
        , environment = ""
        , fingerprint = Nothing
        , framework = Nothing
        , language = Hardcoded
        , level = Just Error
        , notifier =
            Just $ Notifier Hardcoded Paths_wai_middleware_rollbar.version
        , person = Nothing
        , platform = Just . Platform $ T.pack os
        , request = Nothing
        , server = Nothing
        , timestamp = Nothing
        , title = Nothing
        , uuid = Nothing
        }

newtype Environment
    = Environment T.Text
    deriving (Eq, IsString, Show, ToJSON)

data Body arbitrary
    = Message
        { body  :: Body'
        , data' :: arbitrary
        }
    deriving (Eq, Generic, Show)

bodyKVs :: (KeyValue kv, ToJSON v) => Body v -> [kv]
bodyKVs Message{..} =
    [ "body" .= body
    , "data" .= data'
    ]

instance ToJSON arbitrary => ToJSON (Body arbitrary) where
    toJSON = object . bodyKVs
    toEncoding = pairs . mconcat . bodyKVs

newtype Body'
    = Body' T.Text
    deriving (Eq, IsString, Show, ToJSON)

data Level
    = Debug
    | Info
    | Warning
    | Error
    | Critical
    deriving (Eq, Generic, Show)

instance ToJSON Level where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = (toLower <$>)}
    toEncoding = genericToEncoding defaultOptions { constructorTagModifier = (toLower <$>)}

data CodeVersion
    = SemVer T.Text
    | Number Int
    | SHA T.Text
    deriving (Eq, Generic, Show)

prettyCodeVersion :: CodeVersion -> T.Text
prettyCodeVersion (SemVer s) = s
prettyCodeVersion (Number n) = T.pack . show $ n
prettyCodeVersion (SHA h)    = h

instance ToJSON CodeVersion where
    toJSON = toJSON . prettyCodeVersion
    toEncoding = toEncoding . prettyCodeVersion

newtype Platform
    = Platform T.Text
    deriving (Eq, IsString, Show, ToJSON)

newtype Framework
    = Framework T.Text
    deriving (Eq, IsString, Show, ToJSON)

newtype Context
    = Context T.Text
    deriving (Eq, IsString, Show, ToJSON)

data Request
    = Request
        { body        :: RawBody
        , get         :: Get
        , headers     :: Headers
        , method      :: Method
        , queryString :: QueryString
        , url         :: URL
        , userIP      :: IP
        }
    deriving (Eq, Generic, Show)

newtype RawBody
    = RawBody BS.ByteString
    deriving (Eq, Generic, IsString, Show)

instance ToJSON RawBody where
    toJSON (RawBody body) = toJSON (myDecodeUtf8 body)
    toEncoding (RawBody body) = toEncoding (myDecodeUtf8 body)

newtype Get
    = Get Query
    deriving (Eq, Generic, Show)

instance ToJSON Get where
    toJSON (Get q) = toJSON (toJSONQuery q)
    -- TODO: Implement more efficient version
    toEncoding (Get q) = toEncoding (toJSONQuery q)

newtype Headers
    = Headers RequestHeaders
    deriving (Eq, Generic, Show)

instance ToJSON Headers where
    toJSON (Headers hs) = toJSON (toJSONRequestHeaders hs)
    -- TODO: Implement more efficient version
    toEncoding (Headers hs) = toEncoding (toJSONRequestHeaders hs)

newtype Method
    = Method BS.ByteString
    deriving (Eq, Generic, Show)

instance ToJSON Method where
    toJSON (Method q) = toJSON (myDecodeUtf8 q)
    toEncoding (Method q) = toEncoding (myDecodeUtf8 q)

newtype QueryString
    = QueryString BS.ByteString
    deriving (Eq, Generic, Show)

instance ToJSON QueryString where
    toJSON (QueryString q) = toJSON (fromMaybe "" (myDecodeUtf8 q))
    toEncoding (QueryString q) = toEncoding (fromMaybe "" (myDecodeUtf8 q))

newtype IP
    = IP SockAddr
    deriving (Eq, Generic, Show)

instance ToJSON IP where
    toJSON (IP ip) = toJSON (show ip)
    toEncoding (IP ip) = toEncoding (show ip)

requestKVs :: KeyValue kv => Request -> [kv]
requestKVs Request{..} =
    [ "body" .= body
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

myDecodeUtf8 :: BS.ByteString -> Maybe T.Text
myDecodeUtf8 = either (const Nothing) Just . TE.decodeUtf8'

toJSONQuery :: Query -> Value
toJSONQuery = object . mapMaybe go
    where
    go :: (BS.ByteString, Maybe BS.ByteString) -> Maybe (T.Text, Value)
    go (key', val') = do
        key <- myDecodeUtf8 key'
        let val = val' >>= myDecodeUtf8
        pure (key, toJSON val)

toJSONRequestHeaders :: RequestHeaders -> Value
toJSONRequestHeaders = object . mapMaybe go
    where
    go :: Header -> Maybe (T.Text, Value)
    go (key', val') = do
        key <- myDecodeUtf8 $ original key'
        val <- myDecodeUtf8 val'
        pure (T.pack . show $ key, toJSON val)

newtype URL
    = URL (Maybe BS.ByteString, [T.Text])
    deriving (Eq, Generic, Show)

prettyURL :: URL -> T.Text
prettyURL (URL (Nothing, parts)) = T.intercalate "/" parts
prettyURL (URL (Just host', parts)) = T.intercalate "/" (host : parts)
    where
    host = fromMaybe "" (myDecodeUtf8 host')

instance ToJSON URL where
    toJSON = toJSON . prettyURL
    toEncoding = toEncoding . prettyURL

data Person
    = Person
        { id       :: Id
        , username :: Maybe Username
        , email    :: Maybe Email
        }
    deriving (Eq, Generic, Show)

instance ToJSON Person where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
    toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

newtype Id
    = Id T.Text
    deriving (Eq, IsString, Show, ToJSON)

newtype Username
    = Username T.Text
    deriving (Eq, IsString, Show, ToJSON)

newtype Email
    = Email T.Text
    deriving (Eq, IsString, Show, ToJSON)

data Server
    = Server
        { host        :: Maybe HostName
        , root        :: Maybe Root
        , branch      :: Maybe Branch
        , codeVersion :: Maybe CodeVersion
        }
    deriving (Eq, Generic, Show)

instance ToJSON Server where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
    toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

newtype Root
    = Root T.Text
    deriving (Eq, IsString, Show, ToJSON)

newtype Branch
    = Branch T.Text
    deriving (Eq, IsString, Show, ToJSON)

newtype Fingerprint
    = Fingerprint T.Text
    deriving (Eq, IsString, Show, ToJSON)

newtype Title
    = Title T.Text
    deriving (Eq, IsString, Show, ToJSON)

data Notifier
    = Notifier
        { name    :: Hardcoded "wai-middleware-rollbar"
        , version :: Version
        }
    deriving (Eq, Generic, Show)

instance ToJSON Notifier where
    toEncoding = genericToEncoding defaultOptions

newtype UUID4
    = UUID4 UUID
    deriving (Eq, Generic, Show)

instance ToJSON UUID4 where
    toJSON (UUID4 uuid) = toJSON (toText uuid)
    toEncoding (UUID4 uuid) = toEncoding (toText uuid)

data Hardcoded (symbol :: Symbol)
    = Hardcoded
    deriving (Eq, Generic, Show)

instance KnownSymbol symbol => ToJSON (Hardcoded symbol) where
    toJSON = toJSON . symbolVal
    toEncoding = toEncoding . symbolVal
