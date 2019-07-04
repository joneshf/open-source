{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Go.Format (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( Rules
    , cmd_
    , copyFileChanged
    , need
    , needed
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath (dropDirectory1, dropExtension)
import "base" GHC.Records                 (HasField(getField))

rules :: (HasField "buildDir" e FilePath) => ReaderT e Rules ()
rules = do
  buildDir <- asks (getField @"buildDir")

  lift $ buildDir <//> "*.go.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need ["Shake/Go/Format.hs"]
    cmd_ "gofmt" "-w" [input]
    copyFileChanged input out
    needed [input]
