{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.Executable (rules) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd, Traced)
    , Rules
    , cmd_
    , copyFileChanged
    , getDirectoryFiles
    , need
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath ((</>))
import "base" GHC.Records                 (HasField(getField))
import "this" Shake.Package.Haskell
    ( Executable(Executable, executableDirectory, executableName)
    )
import "directory" System.Directory       (getCurrentDirectory)

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  FilePath ->
  Executable ->
  ReaderT e Rules ()
rules name sourceDirectory = \case
  Executable { executableDirectory, executableName } -> do
    binDir <- asks (getField @"binDir")
    buildDir <- asks (getField @"buildDir")
    packageDir <- asks (getField @"packageDir")
    root <- liftIO getCurrentDirectory
    let build' = buildDir </> package'
        package' = packageDir </> name

    lift $ binDir </> executableName %> \out -> do
      let binary = build' </> "build" </> executableName </> executableName
      need ["Shake/Haskell/Executable.hs", binary]
      copyFileChanged binary out

    lift $ build' </> "build" </> executableName </> executableName %> \_ -> do
      srcs <-
        getDirectoryFiles
          ""
          [ package' </> sourceDirectory <//> "*.hs"
          , package' </> executableDirectory <//> "*.hs"
          ]
      need ("Shake/Haskell/Executable.hs" : (build' </> ".build") : srcs)
      cmd_
        (Cwd package')
        (Traced "cabal build")
        "cabal v1-build"
        ["exe:" <> executableName]
        "--builddir"
        [root </> build']
