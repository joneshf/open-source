{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.Format (rules) where

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
import "shake" Development.Shake.FilePath (dropDirectory1, dropExtension)
import "base" GHC.Records                 (HasField(getField))

rules :: (HasField "buildDir" e FilePath) => ReaderT e Rules ()
rules = do
  buildDir <- asks (getField @"buildDir")
  lift $ buildDir <//> "*.hs.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".stylish-haskell.yaml"]
    cmd_ (Traced "stylish-haskell") "stylish-haskell" "--inplace" [input]
    copyFileChanged input out
    needed ["Shake/Haskell.hs", input]
