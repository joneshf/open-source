{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.Test (rules) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd, FileStdout, Traced)
    , Rules
    , cmd_
    , getDirectoryFiles
    , need
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath (dropExtension, (<.>), (</>))
import "base" GHC.Records                 (HasField(getField))
import "this" Shake.Package.Haskell       (Test(Test, suite, testDirectory))
import "directory" System.Directory       (getCurrentDirectory)

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  FilePath ->
  Test ->
  ReaderT e Rules ()
rules name sourceDirectory = \case
  Test { testDirectory, suite } -> do
    buildDir <- asks (getField @"buildDir")
    packageDir <- asks (getField @"packageDir")
    root <- liftIO getCurrentDirectory
    let build' = buildDir </> package'
        package' = packageDir </> name

    lift $ build' </> "build" </> suite </> suite %> \_ -> do
      srcs <-
        getDirectoryFiles
          ""
          [ package' </> sourceDirectory <//> "*.hs"
          , package' </> testDirectory </> suite <//> "*.hs"
          ]
      need ("Shake/Haskell/Test.hs" : (build' </> ".build") : srcs)
      cmd_
        (Cwd package')
        (Traced "cabal build")
        "cabal v1-build"
        ["test:" <> suite]
        "--builddir"
        [root </> build']

    lift $ build' </> "build" </> suite </> suite <.> "out" %> \out -> do
      need ["Shake/Haskell/Test.hs", dropExtension out]
      cmd_
        (Cwd package')
        (FileStdout out)
        (Traced $ name <> " " <> suite)
        [root </> dropExtension out]
