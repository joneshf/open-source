{-# LANGUAGE DataKinds #-}
module Rollbar.Golden where

import Data.Proxy (Proxy(Proxy))

import Rollbar.Item       (Item)
import Rollbar.QuickCheck ()

import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec              (hspec)

main :: IO ()
main =
  hspec $ roundtripAndGoldenSpecs (Proxy :: Proxy (Item () '["Authorization"]))
