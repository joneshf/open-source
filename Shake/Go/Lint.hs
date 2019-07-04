{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Go.Lint (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Traced)
    , Rules
    , cmd_
    , copyFileChanged
    , need
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

  lift $ buildDir <//> "*.go.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need ["Shake/Go/Lint.hs", input, out -<.> "format"]
    cmd_ (Traced "golint") "golint" "-set_exit_status" [input]
    copyFileChanged input out
