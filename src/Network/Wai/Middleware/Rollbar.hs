{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

module Network.Wai.Middleware.Rollbar (requests) where

import Control.Concurrent (forkIO)
import Control.Exception  (Handler(Handler), catches, displayException)
import Control.Monad      (when)

import Data.Aeson   (ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Functor (void)
import Data.Maybe   (fromMaybe)
import Data.Time    (getCurrentTime)
import Data.UUID.V4 (nextRandom)

import GHC.Generics (Generic)

import Network.HostName          (getHostName)
import Network.HTTP.Client       (HttpException)
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
    (Status(Status), statusCode, statusIsServerError, statusMessage)
import Network.Wai               (Middleware, ResponseReceived)

import System.Environment (getExecutablePath)
import System.IO          (hPutStrLn, stderr)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Network.Wai        as NW
import qualified Rollbar.Item       as RI

-- | Middleware that watches responses
--  and sends an item to Rollbar if it is a server error (5xx).
--
--  Sends additional metadata including the request information.
requests :: RI.AccessToken -> RI.Environment -> Middleware
requests accessToken environment app req handler' = app req handler
    where
    handler :: NW.Response -> IO ResponseReceived
    handler res = do
        _ <- forkIO $ handle500 accessToken environment req res
        handler' res

handle500 :: RI.AccessToken -> RI.Environment -> NW.Request -> NW.Response -> IO ()
handle500 accessToken environment req res =
    when (statusIsServerError $ NW.responseStatus res) (send accessToken environment req res)
        `catches`
            [ Handler handleHttpException
            , Handler handleJSONException
            ]

send :: RI.AccessToken -> RI.Environment -> NW.Request -> NW.Response -> IO ()
send accessToken environment req res = do
    uuid <- Just . RI.UUID4 <$> nextRandom
    timestamp <- Just <$> getCurrentTime
    host <- Just <$> getHostName
    root <- Just . RI.Root . T.pack <$> getExecutablePath
    let request = Just RI.Request {..}
    let server = Just RI.Server { RI.branch = Nothing, RI.serverCodeVersion = Nothing, .. }
    let itemData = (RI.error environment messageBody payload)
            { RI.request, RI.server, RI.timestamp, RI.uuid }
    let rReq = rollbarRequest RI.Item{..}
    void $ httpNoBody rReq
    where
    Status{..} = NW.responseStatus res
    headers = RI.Headers $ NW.requestHeaders req
    messageBody = RI.MessageBody <$> myDecodeUtf8 statusMessage
    rawBody = ""
    get = RI.Get $ NW.queryString req
    method = RI.Method $ NW.requestMethod req
    queryString = RI.QueryString $ NW.rawQueryString req
    url = RI.URL (NW.requestHeaderHost req, NW.pathInfo req)
    userIP = RI.IP $ NW.remoteHost req
    referer = myDecodeUtf8 =<< NW.requestHeaderReferer req
    range = myDecodeUtf8 =<< NW.requestHeaderRange req
    userAgent = myDecodeUtf8 =<< NW.requestHeaderUserAgent req
    payload = Payload
        { statusMessage = myDecodeUtf8' statusMessage
        , ..
        }

    myDecodeUtf8 = either (const Nothing) Just . TE.decodeUtf8'
    myDecodeUtf8' = fromMaybe "" . myDecodeUtf8

handleHttpException :: HttpException -> IO ()
handleHttpException e = do
    hPutStrLn stderr "Ran into an exception while sending a request to Rollbar:"
    hPutStrLn stderr $ displayException e

handleJSONException :: JSONException -> IO ()
handleJSONException e = do
    hPutStrLn stderr "Ran into an exception while parsing JSON response from Rollbar:"
    hPutStrLn stderr $ displayException e

rollbarRequest :: ToJSON a => RI.Item a -> Network.HTTP.Simple.Request
rollbarRequest payload =
    setRequestMethod "POST"
    . setRequestSecure True
    . setRequestHost "api.rollbar.com"
    . setRequestPort 443
    . setRequestPath "api/1/item/"
    . setRequestBodyJSON payload
    . setRequestIgnoreStatus
    $ defaultRequest

data Payload
    = Payload
        { statusCode    :: Int
        , statusMessage :: T.Text
        , userAgent     :: Maybe T.Text
        , range         :: Maybe T.Text
        , referer       :: Maybe T.Text
        }
    deriving (Generic, Show)

instance ToJSON Payload where
    toEncoding = genericToEncoding defaultOptions
