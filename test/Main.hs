{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.QuickCheck (conjoin, quickCheck)

import qualified Rollbar.Item.Data.Test

main :: IO ()
main =
    quickCheck $ conjoin
        [ Rollbar.Item.Data.Test.props
        ]
