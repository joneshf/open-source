{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Yaml (rules) where

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
  lift $ buildDir <//> "*.yaml.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".prettierrc", "Shake/Yaml.hs"]
    cmd_ (Traced "prettier") "prettier" "--write" [input]
    copyFileChanged input out
    needed [input]

  lift $ buildDir <//> "*.yaml.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".yamllint", "Shake/Yaml.hs", input, out -<.> "format"]
    cmd_ (Traced "yamllint") "yamllint" "--strict" [input]
    copyFileChanged input out
