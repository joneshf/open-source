{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.QuickCheck (conjoin, quickCheck)

import qualified Rollbar.Item.Data.Test
import qualified Rollbar.Item.Request.Test

main :: IO ()
main =
    quickCheck $ conjoin
        [ Rollbar.Item.Data.Test.props
        , Rollbar.Item.Request.Test.props
        ]
