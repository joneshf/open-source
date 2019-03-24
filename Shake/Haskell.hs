{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell (rules) where

import "mtl" Control.Monad.Reader   (ReaderT, asks)
import "base" Data.Foldable         (for_)
import "shake" Development.Shake    (Rules)
import "base" GHC.Records           (HasField(getField))
import "this" Shake.Package.Haskell
    ( Executable
    , Manifest
    , Package(Package)
    , Test
    )

import qualified "this" Shake.Haskell.Build
import qualified "this" Shake.Haskell.Configure
import qualified "this" Shake.Haskell.Executable
import qualified "this" Shake.Haskell.Format
import qualified "this" Shake.Haskell.Lint
import qualified "this" Shake.Haskell.Manifest
import qualified "this" Shake.Haskell.Sdist
import qualified "this" Shake.Haskell.Test
import qualified "this" Shake.Haskell.UploadToHackage
import qualified "this" Shake.Haskell.Watch
import qualified "this" Shake.Package

package ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  [Executable] ->
  Manifest ->
  String ->
  FilePath ->
  [Test] ->
  String ->
  ReaderT e Rules ()
package exes manifest name sourceDirectory tests version = do
  Shake.Haskell.Watch.rules name

  Shake.Haskell.Build.rules name sourceDirectory

  Shake.Haskell.Configure.rules name tests

  for_ exes (Shake.Haskell.Executable.rules name sourceDirectory)

  for_ tests (Shake.Haskell.Test.rules name sourceDirectory)

  Shake.Haskell.UploadToHackage.rules name version

  Shake.Haskell.Sdist.rules name sourceDirectory version

  Shake.Haskell.Manifest.rules name manifest

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , HasField "packages" e [Shake.Package.Package]
  ) =>
  ReaderT e Rules ()
rules = do
  packages <- asks (getField @"packages")

  Shake.Haskell.Format.rules

  Shake.Haskell.Lint.rules

  for_ packages $ \case
    Shake.Package.Haskell
      (Package exes manifest name sourceDirectory tests' version) ->
        package exes manifest name sourceDirectory tests' version
    Shake.Package.JavaScript _ -> pure mempty
