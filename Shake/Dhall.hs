{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Dhall (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
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
import "base" GHC.Records                 (HasField(getField))

rules :: (HasField "buildDir" e FilePath) => ReaderT e Rules ()
rules = do
  buildDir <- asks (getField @"buildDir")
  lift $ buildDir <//> "*.dhall.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need ["Shake/Dhall.hs", out -<.> "lint"]
    cmd_ (Traced "dhall format") "dhall format" "--inplace" [input]
    copyFileChanged input out
    needed [input]

  lift $ buildDir <//> "*.dhall.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need ["Shake/Dhall.hs"]
    cmd_ (Traced "dhall lint") "dhall lint" "--inplace" [input]
    copyFileChanged input out
    needed [input]
