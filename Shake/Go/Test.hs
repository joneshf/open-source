{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Go.Test (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd, FileStdout, Traced)
    , Rules
    , cmd_
    , getDirectoryFiles
    , need
    , (%>)
    )
import "shake" Development.Shake.FilePath ((</>))
import "base" GHC.Records                 (HasField(getField))

rules ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  FilePath ->
  ReaderT e Rules ()
rules name sourceDirectory = do
  buildDir' <- asks (getField @"buildDir")
  packageDir' <- asks (getField @"packageDir")
  let buildDir = buildDir' </> packageDir
      packageDir = packageDir' </> name

  lift $ buildDir </> ".test" %> \out -> do
    srcs <- getDirectoryFiles "" [packageDir </> sourceDirectory </> "*.go"]
    need ("Shake/Go/Test.hs" : srcs)
    cmd_ (Cwd packageDir) (FileStdout out) (Traced "go test") "go test" [sourceDirectory]
