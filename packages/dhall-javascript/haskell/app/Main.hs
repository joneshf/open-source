{-# LANGUAGE PackageImports #-}
module Main where

import qualified "dhall-javascript" Dhall.JavaScript

main :: IO ()
main = Dhall.JavaScript.main
