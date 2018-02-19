{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Katip.Scribes.Rollbar where

import Prelude hiding (error)

import "base" Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import "base" Control.Monad           (replicateM, when)
import "base" Data.Foldable           (for_)
import "base" Data.Functor            (void)
import "base" GHC.Conc                (atomically)

import "async" Control.Concurrent.Async            (async, waitCatch)
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
import "time" Data.Time.Clock                      (UTCTime)
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
    , _itemHost
    , _itemMessage
    , _itemSeverity
    , _itemTime
    )
import "hostname" Network.HostName                 (HostName)
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
import "rollbar-hs" Rollbar.Item.CodeVersion       (CodeVersion)
import "rollbar-hs" Rollbar.Item.Data
    ( Data(framework, server, timestamp)
    )
import "rollbar-hs" Rollbar.Item.Environment       (Environment(Environment))
import "rollbar-hs" Rollbar.Item.MissingHeaders    (RemoveHeaders)
import "rollbar-hs" Rollbar.Item.Server
    ( Branch
    , Server(Server, branch, host, root, serverCodeVersion)
    )

import qualified "katip" Katip

queueSize :: Int
queueSize = 10

workerSize :: Int
workerSize = 3

mkRollbarScribe ::
  RemoveHeaders headers =>
  proxy headers ->
  AccessToken ->
  Maybe Branch ->
  Maybe CodeVersion ->
  Manager ->
  Severity ->
  Verbosity ->
  IO Scribe
mkRollbarScribe proxy accessToken branch codeVersion manager severity verbosity = do
  queue <- newTBMQueueIO queueSize
  finalize <- newEmptyMVar
  setupWorkers proxy queue finalize manager
  let liPush item = when (permitItem severity item) $
        atomically (writeTBMQueue queue $ rollbarItem' item)
      rollbarItem' item = rollbarItem proxy accessToken branch codeVersion verbosity item
      scribeFinalizer = do
        putMVar finalize ()
        takeMVar finalize
  pure Scribe { liPush, scribeFinalizer }

rollbarItem ::
  (LogItem a, RemoveHeaders headers) =>
  proxy headers ->
  AccessToken ->
  Maybe Branch ->
  Maybe CodeVersion ->
  Verbosity ->
  Katip.Item a ->
  Item Value ("Authorization" ': headers)
rollbarItem _ accessToken branch serverCodeVersion verbosity item =
  Item { accessToken, itemData }
  where
  environment :: Environment
  environment = Environment (getEnvironment $ _itemEnv item)
  hostName :: HostName
  hostName = _itemHost item
  itemData :: Data Value ("Authorization" ': headers)
  itemData = itemData'
    { framework = Just "katip"
    , server = Just server
    , timestamp = Just timestamp
    }
  itemData' :: Data Value ("Authorization" ': headers)
  itemData' = case severity of
    DebugS   -> debug environment (Just messageBody) value
    InfoS    -> info environment (Just messageBody) value
    NoticeS  -> info environment (Just messageBody) value
    WarningS -> warning environment (Just messageBody) value
    ErrorS   -> error environment (Just messageBody) value
    _        -> critical environment (Just messageBody) value
  messageBody :: MessageBody
  messageBody =
    MessageBody (toStrict $ toLazyText $ unLogStr $ _itemMessage item)
  server :: Server
  server = Server { branch, host = Just hostName, root = Nothing, serverCodeVersion }
  severity :: Severity
  severity = _itemSeverity item
  timestamp :: UTCTime
  timestamp = _itemTime item
  value :: Value
  value = itemJson verbosity item

-- | Setup the workers and wait until the finalizer is evaluated.
--   Then, close the queueu, wait for the workers and continue finalization.
setupWorkers ::
  RemoveHeaders headers =>
  proxy headers ->
  TBMQueue (Item Value ("Authorization" ': headers)) ->
  MVar () ->
  Manager ->
  IO ()
setupWorkers proxy queue finalize manager = void $ async $ do
  workers <- replicateM workerSize (async $ mkWorker proxy manager queue)
  takeMVar finalize
  atomically (closeTBMQueue queue)
  for_ workers waitCatch
  putMVar finalize ()

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
