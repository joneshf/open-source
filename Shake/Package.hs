{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Package
  ( Package(..)
  , Manifest(..)
  , Test(..)
  , inputs
  , rules
  , writeDhall
  ) where

import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "text" Data.Text                   (unpack)
import "shake" Development.Shake
    ( FilePattern
    , Rules
    , getDirectoryFilesIO
    , need
    , phony
    , want
    , writeFileChanged
    , (<//>)
    )
import "shake" Development.Shake.FilePath ((<.>), (</>))
import "dhall" Dhall                      (Interpret, Type(expected), auto)
import "dhall" Dhall.Core                 (pretty)
import "base" GHC.Generics                (Generic)
import "base" GHC.Records                 (HasField(getField))

data Manifest
  = Cabal
  | Hpack
  deriving (Generic)

instance Interpret Manifest

data Package
  = Haskell
    { manifest        :: Manifest
    , name            :: String
    , sourceDirectory :: FilePath
    , tests           :: [Test]
    , version         :: String
    }
  deriving (Generic)

instance Interpret Package

data Test
  = Test
    { suite         :: String
    , testDirectory :: FilePath
    }
  deriving (Generic)

instance Interpret Test

(<->) :: FilePath -> FilePath -> FilePath
x <-> y = x <> "-" <> y

build :: FilePath -> FilePath -> Package ->  FilePath
build buildDir packageDir = \case
  Haskell { name } -> buildDir </> packageDir </> name </> ".build"

inputs :: FilePath -> Package -> [FilePattern]
inputs packageDir = \case
  Haskell { manifest, name, sourceDirectory, tests } ->
    config : manifestInput manifest : sourceInput : fmap testInput tests
    where
    config = packageDir </> name </> "shake.dhall"
    manifestInput = \case
      Cabal -> packageDir </> name </> name <.> "cabal"
      Hpack -> packageDir </> name </> "package.yaml"
    sourceInput = packageDir </> name </> sourceDirectory <//> "*.hs"
    testInput = \case
      Test { testDirectory } ->
        packageDir </> name </> testDirectory <//> "*.hs"

rules ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , HasField "packages" e [Package]
  ) =>
  ReaderT e Rules ()
rules = do
  buildDir <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  packages <- asks (getField @"packages")
  allFiles <-
    liftIO
      ( getDirectoryFilesIO
        ""
        ("Shakefile.hs" : "Shake//*.hs" : foldMap (inputs packageDir) packages)
      )
  let buildNeeds = fmap (build buildDir packageDir) packages
      ciNeeds =
        buildNeeds <> formatNeeds <> lintNeeds <> sdistNeeds <> testNeeds
      formatNeeds = fmap (\x -> buildDir </> x <.> "format") allFiles
      lintNeeds = fmap (\x -> buildDir </> x <.> "lint") allFiles
      sdistNeeds = fmap (sdist buildDir packageDir) packages
      testNeeds = foldMap (test buildDir packageDir) packages
      uploadToHackageNeeds = fmap (uploadToHackage buildDir packageDir) packages

  lift $ want ["build"]

  lift $ phony "build" (need buildNeeds)

  lift $ phony "ci" (need ciNeeds)

  lift $ phony "format" (need formatNeeds)

  lift $ phony "lint" (need lintNeeds)

  lift $ phony "sdist" (need sdistNeeds)

  lift $ phony "test" (need testNeeds)

  lift $ phony "upload-to-hackage" (need uploadToHackageNeeds)

sdist :: FilePath -> FilePath -> Package -> FilePath
sdist buildDir packageDir = \case
  Haskell { name, version } ->
    buildDir </> packageDir </> name </> name <-> version <.> "tar.gz"

test :: FilePath -> FilePath -> Package -> [FilePath]
test buildDir packageDir = \case
  Haskell { name, tests } -> fmap go tests
    where
    go = \case
      Test { suite } ->
        buildDir
          </> packageDir
          </> name
          </> "build"
          </> suite
          </> suite
          <.> "out"

uploadToHackage :: FilePath -> FilePath -> Package -> FilePath
uploadToHackage buildDir packageDir = \case
  Haskell { name, version } ->
    buildDir </> packageDir </> name </> name <-> version

writeDhall :: IO ()
writeDhall = do
  writeFileChanged "Manifest.dhall" (unpack $ pretty $ expected $ auto @Manifest)
  writeFileChanged "Package.dhall" (unpack $ pretty $ expected $ auto @Package)
  writeFileChanged "Test.dhall" (unpack $ pretty $ expected $ auto @Test)
