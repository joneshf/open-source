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

binaries :: FilePath -> Package -> [FilePath]
binaries binDir = \case
  Haskell package -> Shake.Package.Haskell.binaries binDir package

build :: FilePath -> FilePath -> Package ->  FilePath
build buildDir packageDir = \case
  Haskell package -> Shake.Package.Haskell.build buildDir packageDir package

executable :: FilePath -> FilePath -> Package -> [FilePath]
executable buildDir packageDir = \case
  Haskell package -> Shake.Package.Haskell.executable buildDir packageDir package

inputs :: FilePath -> Package -> [FilePattern]
inputs packageDir = \case
  Haskell package -> Shake.Package.Haskell.inputs packageDir package

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
  binDir <- asks (getField @"binDir")
  buildDir <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  packages <- asks (getField @"packages")
  allFiles <-
    liftIO
      ( getDirectoryFilesIO
        ""
        ("Shakefile.hs" : "Shake//*.hs" : foldMap (inputs packageDir) packages)
      )
  let binariesNeeds = foldMap (binaries binDir) packages
      buildNeeds = fmap (build buildDir packageDir) packages
      ciNeeds =
        buildNeeds
          <> executableNeeds
          <> formatNeeds
          <> lintNeeds
          <> sdistNeeds
          <> testNeeds
      executableNeeds = foldMap (executable buildDir packageDir) packages
      formatNeeds = fmap (\x -> buildDir </> x <.> "format") allFiles
      lintNeeds = fmap (\x -> buildDir </> x <.> "lint") allFiles
      sdistNeeds = fmap (sdist buildDir packageDir) packages
      testNeeds = foldMap (test buildDir packageDir) packages
      uploadToHackageNeeds = fmap (uploadToHackage buildDir packageDir) packages

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

sdist :: FilePath -> FilePath -> Package -> FilePath
sdist buildDir packageDir = \case
  Haskell package -> Shake.Package.Haskell.sdist buildDir packageDir package

test :: FilePath -> FilePath -> Package -> [FilePath]
test buildDir packageDir = \case
  Haskell package -> Shake.Package.Haskell.test buildDir packageDir package

uploadToHackage :: FilePath -> FilePath -> Package -> FilePath
uploadToHackage buildDir packageDir = \case
  Haskell package ->
    Shake.Package.Haskell.uploadToHackage buildDir packageDir package

writeDhall :: IO ()
writeDhall = do
  Shake.Package.Haskell.writeDhall
  writeFileChanged "Package.dhall" (unpack $ pretty $ expected packageType)
