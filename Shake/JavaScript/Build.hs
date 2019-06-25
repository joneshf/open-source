{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.JavaScript.Build (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd, FileStdout, Traced)
    , Rules
    , alwaysRerun
    , cmd_
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
  ReaderT e Rules ()
rules name = do
  buildDir' <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  let buildDir = buildDir' </> packageDir </> name

  lift $ buildDir </> ".build" %> \out -> do
    need ["Shake/JavaScript/Build.hs", buildDir </> "package.json"]
    alwaysRerun
    cmd_ (Cwd buildDir) (FileStdout out) (Traced "yarn install") "yarn install"
