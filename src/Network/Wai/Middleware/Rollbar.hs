{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Middleware.Rollbar where

import Control.Concurrent     (forkIO)
import Control.Exception      (Handler(Handler), catches, displayException)
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)

import Data.Aeson   (ToJSON)
import Data.Functor (void)

import Network.HTTP.Client       (HttpException)
import Network.HTTP.Simple
    ( JSONException
    , Request
    , defaultRequest
    , httpNoBody
    , parseRequest_
    , setRequestBodyJSON
    , setRequestHost
    , setRequestIgnoreStatus
    , setRequestMethod
    , setRequestPath
    , setRequestPort
    , setRequestSecure
    )
import Network.HTTP.Types.Status (statusCode, statusIsServerError)
import Network.Wai
    (Middleware, Response, ResponseReceived, responseStatus)

import Rollbar.Item (AccessToken, Item(..), error)

import System.IO (hPrint, hPutStrLn, stderr)

requests :: AccessToken -> Middleware
requests accessToken app req handler' = app req handler
    where
    handler :: Response -> IO ResponseReceived
    handler res = do
        _ <- forkIO $ handle500 accessToken res
        handler' res

handle500 :: AccessToken -> Response -> IO ()
handle500 accessToken res =
    when (statusIsServerError status) (void $ httpNoBody request) `catches`
        [ Handler (handleHttpException request)
        , Handler (handleJSONException request)
        ]
    where
    request = rollbarRequest Item{..}
    data' = Rollbar.Item.error code
    status = responseStatus res
    code = statusCode status

handleHttpException :: Request -> HttpException -> IO ()
handleHttpException req e = do
    hPutStrLn stderr "Ran into an exception while sending a request to Rollbar:"
    hPutStrLn stderr $ displayException e
    hPutStrLn stderr "This is the original request:"
    hPrint stderr req

handleJSONException :: Request -> JSONException -> IO ()
handleJSONException req e = do
    hPutStrLn stderr "Ran into an exception while parsing JSON response from Rollbar:"
    hPutStrLn stderr $ displayException e
    hPutStrLn stderr "This is the original request:"
    hPrint stderr req

rollbarRequest :: ToJSON a => Item a -> Request
rollbarRequest payload =
    setRequestMethod "POST"
    . setRequestSecure True
    . setRequestHost "api.rollbar.com"
    . setRequestPort 443
    . setRequestPath "api/1/item/"
    . setRequestBodyJSON payload
    . setRequestIgnoreStatus
    $ defaultRequest
