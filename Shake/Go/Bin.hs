{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Go.Bin (rules) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(AddEnv, Cwd, Traced)
    , Rules
    , cmd_
    , getDirectoryFiles
    , need
    , (%>)
    )
import "shake" Development.Shake.FilePath (takeDirectory, takeFileName, (</>))
import "base" GHC.Records                 (HasField(getField))
import "this" Shake.Package.Go            (Bin(Bin, file, name))
import "directory" System.Directory       (getCurrentDirectory)

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  FilePath ->
  Bin ->
  ReaderT e Rules ()
rules name' sourceDirectory = \case
  Bin { file, name } -> do
    binDir <- asks (getField @"binDir")
    buildDir' <- asks (getField @"buildDir")
    packageDir' <- asks (getField @"packageDir")
    root <- liftIO getCurrentDirectory
    let buildDir = buildDir' </> packageDir
        packageDir = packageDir' </> name'

    lift $ binDir </> name </> "*/*" %> \out -> do
      let os = (takeFileName . takeDirectory) out
      srcs <- getDirectoryFiles "" [packageDir </> sourceDirectory </> "*.go"]
      need ("Shake/Go/Bin.hs" : (buildDir </> ".build") : srcs)
      cmd_
        (AddEnv "GOOS" os)
        (Cwd packageDir)
        (Traced "go build")
        "go build"
        "-o"
        [root </> out]
        [file]
