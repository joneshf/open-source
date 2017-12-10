{-# LANGUAGE OverloadedStrings #-}

{-|
    Module      : Rollbar.API
    Description : Codifies Rollbar's API
    Copyright   : (c) Hardy Jones, 2017
    License     : BSD3
    Maintainer  : jones3.hardy@gmail.com
    Stability   : experimental

    Provides functions for communicating with Rollbar through the public API.

    See Rollbar's <https://rollbar.com/docs/api/ API> for more details.
-}
module Rollbar.API
    ( itemsPOST
    , itemsPOST'
    , itemsPOSTWithException
    , makeRequest
    ) where

import Control.Monad.IO.Class (MonadIO)

import Data.Aeson (FromJSON, ToJSON)

import Network.HTTP.Client
    ( Manager
    , Request(host, method, path, port, secure)
    , Response
    , defaultRequest
    , setRequestIgnoreStatus
    )
import Network.HTTP.Simple
    ( JSONException
    , httpJSON
    , httpJSONEither
    , setRequestBodyJSON
    , setRequestManager
    )
import Rollbar.Item         (Item, RemoveHeaders)

-- | Sends an 'Rollbar.Item.Item' off to Rollbar.
--
--   Creates a new 'Network.HTTP.Client.Manager' to send off the request.
--   Makes no claims about what you get back.
itemsPOST
    :: (FromJSON c, MonadIO f, RemoveHeaders b, ToJSON a)
    => Item a b
    -> f (Response (Either JSONException c))
itemsPOST = httpJSONEither . makeRequest

-- | Sends an 'Rollbar.Item.Item' off to Rollbar.
--
--   Makes no claims about what you get back.
itemsPOST'
    :: (FromJSON c, MonadIO f, RemoveHeaders b, ToJSON a)
    => Manager
    -> Item a b
    -> f (Response (Either JSONException c))
itemsPOST' manager = httpJSONEither . setRequestManager manager . makeRequest

-- | Sends an 'Rollbar.Item.Item' off to Rollbar.
--
--   Creates a new 'Network.HTTP.Client.Manager' to send off the request.
--   Makes no claims about what you get back.
--   Throws a 'Network.HTTP.Simple.JSONException' if it cannot parse the response.
--
--   Yes, this name is annoying, so are exceptions.
itemsPOSTWithException
    :: (FromJSON c, MonadIO f, RemoveHeaders b, ToJSON a)
    => Item a b
    -> f (Response c)
itemsPOSTWithException = httpJSON . makeRequest

-- | Converts an item into a request ready to send to Rollbar.
--
--   If you need a different scheme for sending items,
--   you'll probably want to use this along with a function like 'Network.HTTP.Client.httpLbs'
--   or 'Network.HTTP.Simple.httpLbs'.
--
--   If you want the JSON back and already have a 'Network.HTTP.Client.Manager',
--   you can use this function with 'Network.HTTP.Simple.setRequestManager'.
--   Then send off the request with something like 'Network.HTTP.Simple.httpJSONEither'.
makeRequest :: (RemoveHeaders headers, ToJSON a) => Item a headers -> Request
makeRequest payload =
    setRequestBodyJSON payload
        . setRequestIgnoreStatus
        $ defaultRequest
            { host = "api.rollbar.com"
            , method = "POST"
            , path = "api/1/item/"
            , port = 443
            , secure = True
            }
