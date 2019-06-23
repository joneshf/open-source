{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.Sdist (rules) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Cwd, Traced)
    , Rules
    , cmd_
    , getDirectoryFiles
    , need
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath ((<.>), (</>))
import "base" GHC.Records                 (HasField(getField))
import "directory" System.Directory       (getCurrentDirectory)

(<->) :: FilePath -> FilePath -> FilePath
x <-> y = x <> "-" <> y

rules ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  FilePath ->
  String ->
  ReaderT e Rules ()
rules name sourceDirectory version = do
  buildDir <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  root <- liftIO getCurrentDirectory
  let build' = buildDir </> package'
      package' = packageDir </> name

  lift $ build' </> name <-> version <.> "tar.gz" %> \_ -> do
    srcs <- getDirectoryFiles "" [package' </> sourceDirectory <//> "*.hs"]
    need
      ( "Shake/Haskell/Sdist.hs"
      : (build' </> name <.> "cabal.lint")
      : (build' </> ".configure")
      : srcs
      )
    cmd_
      (Cwd package')
      (Traced "cabal sdist")
      "cabal v1-sdist"
      "--builddir"
      [root </> build']
