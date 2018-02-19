{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators  #-}

module Katip.Scribes.Rollbar where

import Prelude hiding (error)

import "base" Control.Exception (SomeException, Exception, catch)
import "base" Control.Monad (when)
import "base" Data.Functor  (void)

import "aeson" Data.Aeson                       (Value)
import "text" Data.Text.Lazy                    (toStrict)
import "text" Data.Text.Lazy.Builder            (toLazyText)
import "katip" Katip
    ( LogItem
    , Scribe(Scribe, liPush, scribeFinalizer)
    , Severity(DebugS, ErrorS, InfoS, NoticeS, WarningS)
    , Verbosity
    , getEnvironment
    , itemJson
    , payloadObject
    , permitItem
    , unLogStr
    , _itemEnv
    , _itemMessage
    , _itemSeverity
    )
import "http-client" Network.HTTP.Client        (Manager)
import "rollbar-hs" Rollbar.AccessToken         (AccessToken)
import "rollbar-hs" Rollbar.API                 (itemsPOST')
import "rollbar-hs" Rollbar.Item
    ( Item(Item, accessToken, itemData)
    , critical
    , debug
    , error
    , info
    , warning
    )
import "rollbar-hs" Rollbar.Item.Body           (MessageBody(MessageBody))
import "rollbar-hs" Rollbar.Item.Data           (Data)
import "rollbar-hs" Rollbar.Item.Environment    (Environment(Environment))
import "rollbar-hs" Rollbar.Item.MissingHeaders (RemoveHeaders)

import qualified "katip" Katip

mkRollbarScribe ::
  RemoveHeaders headers =>
  proxy headers ->
  AccessToken ->
  Manager ->
  Severity ->
  Verbosity ->
  IO Scribe
mkRollbarScribe proxy accessToken manager severity verbosity = do
  let liPush item = when (permitItem severity item) $ do
        void (itemsPOST' manager $ rollbarItem' item)
      rollbarItem' item = rollbarItem proxy accessToken item verbosity
      scribeFinalizer = pure ()
  pure Scribe { liPush, scribeFinalizer }

rollbarItem ::
  (LogItem a, RemoveHeaders headers) =>
  proxy headers ->
  AccessToken ->
  Katip.Item a ->
  Verbosity ->
  Item Value ("Authorization" ': headers)
rollbarItem _ accessToken item verbosity = Item { accessToken, itemData }
  where
  environment :: Environment
  environment = Environment (getEnvironment $ _itemEnv item)
  itemData :: Data Value ("Authorization" ': headers)
  itemData = case severity of
    DebugS   -> debug environment (Just messageBody) value
    InfoS    -> info environment (Just messageBody) value
    NoticeS  -> info environment (Just messageBody) value
    WarningS -> warning environment (Just messageBody) value
    ErrorS   -> error environment (Just messageBody) value
    _        -> critical environment (Just messageBody) value
  messageBody :: MessageBody
  messageBody =
    MessageBody (toStrict $ toLazyText $ unLogStr $ _itemMessage item)
  severity :: Severity
  severity = _itemSeverity item
  value :: Value
  value = itemJson verbosity item
