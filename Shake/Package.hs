{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Package
  ( Package(..)
  , inputs
  , packageType
  , rules
  , writeDhall
  ) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "base" Data.Foldable               (fold)
import "text" Data.Text                   (pack, unpack)
import "shake" Development.Shake
    ( FilePattern
    , Rules
    , getDirectoryFilesIO
    , need
    , phony
    , want
    , writeFileChanged
    )
import "shake" Development.Shake.FilePath ((<.>), (</>))
import "dhall" Dhall
    ( Type(expected)
    , auto
    , constructor
    , union
    )
import "dhall" Dhall.Core                 (pretty)
import "base" GHC.Records                 (HasField(getField))

import qualified "this" Shake.Package.Haskell

newtype Package
  = Haskell Shake.Package.Haskell.Package

binaries ::
  ( HasField "binDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
binaries = \case
  Haskell package -> Shake.Package.Haskell.binaries package

build ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f FilePath
build = \case
  Haskell package -> Shake.Package.Haskell.build package

executable ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
executable = \case
  Haskell package -> Shake.Package.Haskell.executable package

inputs ::
  ( HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePattern]
inputs = \case
  Haskell package -> Shake.Package.Haskell.inputs package

packageType :: Type Package
packageType = union (constructor (pack "Haskell") $ fmap Haskell auto)

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , HasField "packages" e [Package]
  ) =>
  ReaderT e Rules ()
rules = do
  buildDir <- asks (getField @"buildDir")
  packages <- asks (getField @"packages")
  inputNeeds <- fmap fold (traverse inputs packages)
  allFiles <-
    liftIO (getDirectoryFilesIO "" ("Shakefile.hs" : "Shake//*.hs" : inputNeeds))
  binariesNeeds <- fmap fold (traverse binaries packages)
  buildNeeds <- traverse build packages
  executableNeeds <- fmap fold (traverse executable packages)
  sdistNeeds <- traverse sdist packages
  testNeeds <- fmap fold (traverse test packages)
  uploadToHackageNeeds <- traverse uploadToHackage packages

  let ciNeeds =
        buildNeeds
          <> executableNeeds
          <> formatNeeds
          <> lintNeeds
          <> sdistNeeds
          <> testNeeds
      formatNeeds = fmap (\x -> buildDir </> x <.> "format") allFiles
      lintNeeds = fmap (\x -> buildDir </> x <.> "lint") allFiles

  lift $ want ["build"]

  lift $ phony "binaries" (need binariesNeeds)

  lift $ phony "build" (need buildNeeds)

  lift $ phony "ci" (need ciNeeds)

  lift $ phony "executable" (need executableNeeds)

  lift $ phony "format" (need formatNeeds)

  lift $ phony "lint" (need lintNeeds)

  lift $ phony "sdist" (need sdistNeeds)

  lift $ phony "test" (need testNeeds)

  lift $ phony "upload-to-hackage" (need uploadToHackageNeeds)

sdist ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f FilePath
sdist = \case
  Haskell package -> Shake.Package.Haskell.sdist package

test ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
test = \case
  Haskell package -> Shake.Package.Haskell.test package

uploadToHackage ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f FilePath
uploadToHackage = \case
  Haskell package ->
    Shake.Package.Haskell.uploadToHackage package

writeDhall :: IO ()
writeDhall = do
  Shake.Package.Haskell.writeDhall
  writeFileChanged "Package.dhall" (unpack $ pretty $ expected packageType)
