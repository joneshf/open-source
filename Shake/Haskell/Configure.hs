{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.Configure (rules) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "base" Data.List.NonEmpty          (nonEmpty)
import "shake" Development.Shake
    ( CmdOption(Cwd, FileStdout, Traced)
    , Rules
    , cmd
    , need
    , (%>)
    )
import "shake" Development.Shake.FilePath ((<.>), (</>))
import "base" GHC.Records                 (HasField(getField))
import "directory" System.Directory       (getCurrentDirectory)

rules ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  [a] ->
  ReaderT e Rules ()
rules name tests = do
  buildDir <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  root <- liftIO getCurrentDirectory
  let build' = buildDir </> package'
      package' = packageDir </> name

  lift $ build' </> ".configure" %> \out -> do
    need
      [ "Shake/Haskell/Configure.hs"
      , buildDir </> ".update"
      , package' </> name <.> "cabal"
      ]
    cmd
      (Cwd package')
      (FileStdout out)
      (Traced "cabal configure")
      "cabal configure"
      "--builddir"
      [root </> build']
      ("--enable-tests" <$ nonEmpty tests)
