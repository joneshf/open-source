{-# LANGUAGE PackageImports #-}
module Shake.Dhall (rules) where

import "shake" Development.Shake
    ( CmdOption(Traced)
    , Rules
    , cmd_
    , copyFileChanged
    , need
    , needed
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath
    ( dropDirectory1
    , dropExtension
    , (-<.>)
    )

rules :: FilePath -> Rules ()
rules buildDir = do
  buildDir <//> "*.dhall.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need ["Shake/Dhall.hs", out -<.> "lint"]
    cmd_ (Traced "dhall format") "dhall format" "--inplace" [input]
    copyFileChanged input out
    needed [input]

  buildDir <//> "*.dhall.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need ["Shake/Dhall.hs"]
    cmd_ (Traced "dhall lint") "dhall lint" "--inplace" [input]
    copyFileChanged input out
    needed [input]
