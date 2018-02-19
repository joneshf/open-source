{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Katip.Scribes.Rollbar where

import Prelude hiding (error)

import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import "base" Control.Monad           (replicateM, when)
import "base" Data.Foldable           (for_)
import "base" Data.Functor            (void)
import "base" GHC.Conc                (atomically)

import "async" Control.Concurrent.Async            (async, waitCatchSTM)
import "stm-chans" Control.Concurrent.STM.TBMQueue
    ( TBMQueue
    , closeTBMQueue
    , newTBMQueueIO
    , readTBMQueue
    , writeTBMQueue
    )
import "aeson" Data.Aeson                          (Value)
import "text" Data.Text.Lazy                       (toStrict)
import "text" Data.Text.Lazy.Builder               (toLazyText)
import "katip" Katip
    ( LogItem
    , Scribe(Scribe, liPush, scribeFinalizer)
    , Severity(DebugS, ErrorS, InfoS, NoticeS, WarningS)
    , Verbosity
    , getEnvironment
    , itemJson
    , permitItem
    , unLogStr
    , _itemEnv
    , _itemMessage
    , _itemSeverity
    )
import "http-client" Network.HTTP.Client           (Manager)
import "rollbar-hs" Rollbar.AccessToken            (AccessToken)
import "rollbar-hs" Rollbar.API                    (itemsPOST')
import "rollbar-hs" Rollbar.Item
    ( Item(Item, accessToken, itemData)
    , critical
    , debug
    , error
    , info
    , warning
    )
import "rollbar-hs" Rollbar.Item.Body              (MessageBody(MessageBody))
import "rollbar-hs" Rollbar.Item.Data              (Data)
import "rollbar-hs" Rollbar.Item.Environment       (Environment(Environment))
import "rollbar-hs" Rollbar.Item.MissingHeaders    (RemoveHeaders)

import qualified "katip" Katip

queueSize :: Int
queueSize = 10

workerSize :: Int
workerSize = 3

mkRollbarScribe ::
  RemoveHeaders headers =>
  proxy headers ->
  AccessToken ->
  Manager ->
  Severity ->
  Verbosity ->
  IO Scribe
mkRollbarScribe proxy accessToken manager severity verbosity = do
  queue <- newTBMQueueIO queueSize
  finalize <- newEmptyMVar
  void $ async $ do
    -- Setup the workers and wait until the finalizer is evaluated.
    -- Then, close the queueu, wait for the workers and continue finalization.
    workers <- replicateM workerSize (async $ mkWorker proxy manager queue)
    takeMVar finalize
    atomically $ do
      closeTBMQueue queue
      for_ workers waitCatchSTM
    putMVar finalize ()
  let liPush item = when (permitItem severity item) $
        atomically (writeTBMQueue queue $ rollbarItem' item)
      rollbarItem' item = rollbarItem proxy accessToken item verbosity
      scribeFinalizer = do
        putMVar finalize ()
        takeMVar finalize
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

mkWorker ::
  (RemoveHeaders headers) =>
  proxy headers ->
  Manager ->
  TBMQueue (Item Value ("Authorization" ': headers)) ->
  IO ()
mkWorker _ manager queue = go
  where
  go = do
    item' <- atomically (readTBMQueue queue)
    for_ item' $ \item -> do
      void (itemsPOST' manager item)
      go
