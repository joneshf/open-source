{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
    Module      : Network.Wai.Middleware.Rollbar
    Description : WAI middleware for interfacing with Rollbar
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    Provides middleware for communicating with Rollbar.

    Currently has middleware for sending all server errors to Rollbar.
    More to come shortly.
-}

module Network.Wai.Middleware.Rollbar (Settings(..), exceptions, requests) where

import Control.Concurrent (forkIO)
import Control.Exception
    ( Handler(Handler)
    , SomeException
    , catches
    , displayException
    , throwIO
    )
import Control.Monad      (when)

import Data.Aeson   (ToJSON)
import Data.Functor (void)
import Data.Maybe   (fromMaybe)
import Data.Time    (getCurrentTime)
import Data.UUID.V4 (nextRandom)

import GHC.TypeLits (Symbol)

import Network.HostName                       (getHostName)
import Network.HTTP.Client                    (HttpException)
import Network.HTTP.Simple
    ( JSONException
    , Request
    , defaultRequest
    , httpNoBody
    , setRequestBodyJSON
    , setRequestHost
    , setRequestIgnoreStatus
    , setRequestMethod
    , setRequestPath
    , setRequestPort
    , setRequestSecure
    )
import Network.HTTP.Types.Status
    ( Status(Status)
    , statusCode
    , statusIsServerError
    , statusMessage
    )
import Network.Wai                            (Middleware, ResponseReceived)
import Network.Wai.Middleware.Rollbar.Payload (Payload)

import System.Environment (getExecutablePath)
import System.IO          (hPutStrLn, stderr)

import qualified Data.ByteString                        as BS
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as TE
import qualified Network.Wai                            as NW
import qualified Network.Wai.Middleware.Rollbar.Payload as Payload
import qualified Rollbar.Item                           as RI

-- | Set up the middleware properly
--  The `headers` are  what you want removed from
--  the request headers sent to Rollbar.
data Settings (headers :: [Symbol])
    = Settings
        { accessToken :: RI.AccessToken
        -- ^ Should have a scope "post_server_item".
        , branch      :: Maybe RI.Branch
        -- ^ Should be the branch of the running application.
        --
        -- Will default to `master` if not set.
        , codeVersion :: Maybe RI.CodeVersion
        -- ^ Should be the version of the running application.
        , environment :: RI.Environment
        -- ^ Should be something meaningful to your program
        --
        -- E.g. "development" or "production"
        }

-- | Middleware that catches exceptions, sends an item to Rollbar,
--  and rethrows the exception.
--
--  Sends additional metadata including the request information.
exceptions :: RI.RemoveHeaders headers => Settings headers -> Middleware
exceptions settings app req handler = app req handler `catches`
    [ Handler $ handleSomeException settings req
    ]

-- | Middleware that watches responses
--  and sends an item to Rollbar if it is a server error (5xx).
--
--  Sends additional metadata including the request information.
requests :: RI.RemoveHeaders headers => Settings headers -> Middleware
requests settings app req handler' = app req handler
    where
    handler :: NW.Response -> IO ResponseReceived
    handler res = do
        _ <- forkIO $ handle500 settings req res
        handler' res

handle500
    :: RI.RemoveHeaders headers
    => Settings headers
    -> NW.Request
    -> NW.Response
    -> IO ()
handle500 settings req res =
    when (statusIsServerError $ NW.responseStatus res) (send settings req res)
        `catches`
            [ Handler handleHttpException
            , Handler handleJSONException
            ]

send
    :: forall headers
    . RI.RemoveHeaders headers
    => Settings headers
    -> NW.Request
    -> NW.Response
    -> IO ()
send settings req res = do
    rReq <- mkRollbarRequest settings req payload messageBody
    void $ httpNoBody rReq
    where
    Status{statusCode, statusMessage} = NW.responseStatus res
    messageBody = RI.MessageBody <$> myDecodeUtf8 statusMessage
    referer = myDecodeUtf8 =<< NW.requestHeaderReferer req
    range = myDecodeUtf8 =<< NW.requestHeaderRange req
    userAgent = myDecodeUtf8 =<< NW.requestHeaderUserAgent req
    payload = Payload.RequestPayload
        { Payload.range
        , Payload.referer
        , Payload.statusCode
        , Payload.statusMessage = myDecodeUtf8' statusMessage
        , Payload.userAgent
        }

handleHttpException :: HttpException -> IO ()
handleHttpException e = do
    hPutStrLn stderr "Ran into an exception while sending a request to Rollbar:"
    hPutStrLn stderr $ displayException e

handleJSONException :: JSONException -> IO ()
handleJSONException e = do
    hPutStrLn stderr "Ran into an exception while parsing JSON response from Rollbar:"
    hPutStrLn stderr $ displayException e

handleSomeException
    :: forall a headers
    . RI.RemoveHeaders headers
    => Settings headers
    -> NW.Request
    -> SomeException
    -> IO a
handleSomeException settings req e = do
    rReq <- mkRollbarRequest settings req payload messageBody
    _ <- httpNoBody rReq
    throwIO e
    where
    exception = T.pack $ displayException e
    messageBody = Just $ RI.MessageBody $
        T.intercalate " "
            [ "Uncaught exception at:"
            , myDecodeUtf8' $ NW.requestMethod req
            , T.intercalate "/" $ NW.pathInfo req
            ]
    referer = myDecodeUtf8 =<< NW.requestHeaderReferer req
    range = myDecodeUtf8 =<< NW.requestHeaderRange req
    userAgent = myDecodeUtf8 =<< NW.requestHeaderUserAgent req
    payload = Payload.ExceptionPayload
      { Payload.exception
      , Payload.referer
      , Payload.range
      , Payload.userAgent
      }

mkRollbarRequest
    :: forall headers
    . RI.RemoveHeaders headers
    => Settings headers
    -> NW.Request
    -> Payload
    -> Maybe RI.MessageBody
    -> IO Request
mkRollbarRequest Settings{accessToken, branch, codeVersion, environment} req payload messageBody = do
    uuid <- Just . RI.UUID4 <$> nextRandom
    timestamp <- Just <$> getCurrentTime
    host <- Just <$> getHostName
    root <- Just . RI.Root . T.pack <$> getExecutablePath
    let request = Just RI.Request
          { RI.get
          , RI.headers
          , RI.method
          , RI.queryString
          , RI.rawBody
          , RI.url
          , RI.userIP
          }
    let server = Just RI.Server
          { RI.branch, RI.host, RI.root, RI.serverCodeVersion = codeVersion }
    let itemData = (RI.error environment messageBody payload)
          { RI.codeVersion, RI.request, RI.server, RI.timestamp, RI.uuid }
    pure $ rollbarRequest RI.Item{RI.accessToken, RI.itemData}
    where
    headers :: RI.MissingHeaders headers
    headers = RI.MissingHeaders $ NW.requestHeaders req
    rawBody = ""
    get = RI.Get $ NW.queryString req
    method = RI.Method $ NW.requestMethod req
    queryString = RI.QueryString $ NW.rawQueryString req
    url = RI.URL (NW.requestHeaderHost req, NW.pathInfo req)
    userIP = RI.IP $ NW.remoteHost req

myDecodeUtf8 :: BS.ByteString -> Maybe T.Text
myDecodeUtf8 = either (const Nothing) Just . TE.decodeUtf8'

myDecodeUtf8' :: BS.ByteString -> T.Text
myDecodeUtf8' = fromMaybe "" . myDecodeUtf8

rollbarRequest
    :: (RI.RemoveHeaders headers, ToJSON a)
    => RI.Item a headers
    -> Network.HTTP.Simple.Request
rollbarRequest payload =
    setRequestMethod "POST"
    . setRequestSecure True
    . setRequestHost "api.rollbar.com"
    . setRequestPort 443
    . setRequestPath "api/1/item/"
    . setRequestBodyJSON payload
    . setRequestIgnoreStatus
    $ defaultRequest
