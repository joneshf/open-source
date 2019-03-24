{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.JavaScript.Format (rules) where

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

  lift $ buildDir <//> "*.js.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".prettierrc", "Shake/JavaScript/Format.hs"]
    cmd_ "prettier" "--write" [input]
    copyFileChanged input out
    needed [input]
