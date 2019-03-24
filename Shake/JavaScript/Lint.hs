{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.JavaScript.Lint (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( Rules
    , cmd_
    , copyFileChanged
    , need
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath (dropDirectory1, dropExtension)
import "base" GHC.Records                 (HasField(getField))

rules :: (HasField "buildDir" e FilePath) => ReaderT e Rules ()
rules = do
  buildDir <- asks (getField @"buildDir")

  lift $ buildDir <//> "*.js.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need ["Shake/JavaScript.hs", input]
    cmd_ "eslint" [input]
    copyFileChanged input out
