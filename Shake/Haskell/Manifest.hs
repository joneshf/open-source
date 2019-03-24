{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.Manifest (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd)
    , Rules
    , cmd_
    , need
    , (%>)
    )
import "shake" Development.Shake.FilePath (replaceFileName, (<.>), (</>))
import "base" GHC.Records                 (HasField(getField))
import "this" Shake.Package.Haskell       (Manifest(Cabal, Hpack))

rules ::
  ( HasField "packageDir" e FilePath
  ) =>
  String ->
  Manifest ->
  ReaderT e Rules ()
rules name = \case
  Cabal -> pure mempty
  Hpack -> do
    packageDir <- asks (getField @"packageDir")
    let package' = packageDir </> name

    lift $ package' </> name <.> "cabal" %> \out -> do
      need ["Shake/Haskell/Manifest.hs", replaceFileName out "package.yaml"]
      cmd_ (Cwd package') "hpack"
