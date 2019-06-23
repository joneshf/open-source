{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.Build (rules) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd, FileStdout, Traced)
    , Rules
    , cmd
    , getDirectoryFiles
    , need
    , (%>)
    , (<//>)
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
  buildDir <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  root <- liftIO getCurrentDirectory
  let build' = buildDir </> package'
      package' = packageDir </> name

  lift $ build' </> ".build" %> \out -> do
    srcs <- getDirectoryFiles "" [package' </> sourceDirectory <//> "*.hs"]
    need ("Shake/Haskell/Build.hs" : (build' </> ".configure") : srcs)
    cmd
      (Cwd package')
      (FileStdout out)
      (Traced "cabal build")
      "cabal v1-build"
      "--builddir"
      [root </> build']
