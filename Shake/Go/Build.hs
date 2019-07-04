{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Go.Build (rules) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd, Traced)
    , Rules
    , cmd_
    , getDirectoryFiles
    , need
    , (%>)
    )
import "shake" Development.Shake.FilePath ((</>))
import "base" GHC.Records                 (HasField(getField))
import "directory" System.Directory       (getCurrentDirectory)

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
  root <- liftIO getCurrentDirectory
  let buildDir = buildDir' </> packageDir
      packageDir = packageDir' </> name

  lift $ buildDir </> ".build" %> \out -> do
    srcs <- getDirectoryFiles "" [packageDir </> sourceDirectory </> "*.go"]
    need ("Shake/Go/Build.hs" : srcs)
    cmd_ (Cwd packageDir) (Traced "go build") "go build" "-o" [root </> out]
