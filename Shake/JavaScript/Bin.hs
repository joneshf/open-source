{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.JavaScript.Bin (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd, Traced, WithStderr)
    , Rules
    , cmd_
    , copyFileChanged
    , need
    , (%>)
    )
import "shake" Development.Shake.FilePath (takeDirectory, takeFileName, (</>))
import "base" GHC.Records                 (HasField(getField))
import "this" Shake.Package.JavaScript    (Bin(Bin, file, name, nodeVersion))

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  Bin ->
  ReaderT e Rules ()
rules name' = \case
  Bin { file, name, nodeVersion } -> do
    binDir <- asks (getField @"binDir")
    buildDir' <- asks (getField @"buildDir")
    packageDir' <- asks (getField @"packageDir")

    let buildDir = buildDir' </> packageDir
        packageDir = packageDir' </> name'

    lift $ binDir </> name </> "*/*" %> \out -> do
      let binary = buildDir </> "bin" </> target </> takeFileName out
          target = (takeFileName . takeDirectory) out
      need ["Shake/JavaScript/Bin.hs", binary]
      copyFileChanged binary out

    lift $ buildDir </> file %> \out -> do
      need ["Shake/JavaScript/Bin.hs", packageDir </> file]
      copyFileChanged (packageDir </> file) out

    lift $ buildDir </> "bin" </> "*/*" %> \out -> do
      let fileName = takeFileName out
          target = (takeFileName . takeDirectory) out
      need
        [ "Shake/JavaScript/Bin.hs"
        , buildDir </> ".build"
        , buildDir </> ".copied"
        , buildDir </> file
        ]

      cmd_
        (Cwd buildDir)
        (Traced "nexe")
        (WithStderr False)
        "yarn run nexe"
        "--input"
        [file]
        "--output"
        ["bin" </> target </> fileName]
        "--target"
        [target <> "-x64-" <> nodeVersion]
