{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Package.Haskell
  ( Executable(..)
  , Manifest(..)
  , Package(..)
  , Test(..)
  , binaries
  , build
  , executable
  , inputs
  , sdist
  , test
  , uploadToHackage
  , writeDhall
  ) where

import "text" Data.Text                   (unpack)
import "shake" Development.Shake
    ( FilePattern
    , writeFileChanged
    , (<//>)
    )
import "shake" Development.Shake.FilePath (exe, (<.>), (</>))
import "dhall" Dhall                      (Interpret, Type(expected), auto)
import "dhall" Dhall.Core                 (pretty)
import "base" GHC.Generics                (Generic)

data Executable
  = Executable
    { executableName      :: String
    , executableDirectory :: FilePath
    }
  deriving (Generic)

instance Interpret Executable

data Manifest
  = Cabal
  | Hpack
  deriving (Generic)

instance Interpret Manifest

data Package
  = Haskell
    { executables     :: [Executable]
    , manifest        :: Manifest
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

binaries :: FilePath -> Package -> [FilePath]
binaries binDir = \case
  Haskell { executables } -> fmap go executables
    where
    go = \case
      Executable { executableName } -> binDir </> executableName

build :: FilePath -> FilePath -> Package ->  FilePath
build buildDir packageDir = \case
  Haskell { name } -> buildDir </> packageDir </> name </> ".build"

executable :: FilePath -> FilePath -> Package -> [FilePath]
executable buildDir packageDir = \case
  Haskell { executables, name } -> fmap go executables
    where
    go = \case
      Executable { executableName } ->
        buildDir
          </> packageDir
          </> name
          </> "build"
          </> executableName
          </> executableName
          <.> exe

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
  writeFileChanged
    "Package/Haskell/Executable.dhall"
    (unpack $ pretty $ expected $ auto @Executable)
  writeFileChanged
    "Package/Haskell/Manifest.dhall"
    (unpack $ pretty $ expected $ auto @Manifest)
  writeFileChanged
    "Package/Haskell/Package.dhall"
    (unpack $ pretty $ expected $ auto @Package)
  writeFileChanged
    "Package/Haskell/Test.dhall"
    (unpack $ pretty $ expected $ auto @Test)
