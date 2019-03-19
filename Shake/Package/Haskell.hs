{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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

import "mtl" Control.Monad.Reader         (ReaderT, ask, asks)
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
import "base" GHC.Records                 (HasField(getField))

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
  = Package
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

binaries ::
  ( HasField "binDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
binaries = \case
  Package { executables } -> traverse go executables
    where
    go = \case
      Executable { executableName } -> do
        env <- ask
        pure (getField @"binDir" env </> executableName)

build ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f FilePath
build = \case
  Package { name } -> do
    env <- ask
    pure
      ( getField @"buildDir" env
        </> getField @"packageDir" env
        </> name
        </> ".build"
      )

executable ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
executable = \case
  Package { executables, name } -> traverse go executables
    where
    go = \case
      Executable { executableName } -> do
        env <- ask
        pure
          ( getField @"buildDir" env
            </> getField @"packageDir" env
            </> name
            </> "build"
            </> executableName
            </> executableName
            <.> exe
          )

inputs ::
  ( HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePattern]
inputs = \case
  Package { manifest, name, sourceDirectory, tests } -> do
    packageDir <- asks (getField @"packageDir")
    pure
      ( config packageDir
        : manifestInput packageDir manifest
        : sourceInput packageDir
        : fmap (testInput packageDir) tests
      )
    where
    config packageDir = packageDir </> name </> "shake.dhall"
    manifestInput packageDir = \case
      Cabal -> packageDir </> name </> name <.> "cabal"
      Hpack -> packageDir </> name </> "package.yaml"
    sourceInput packageDir = packageDir </> name </> sourceDirectory <//> "*.hs"
    testInput packageDir = \case
      Test { testDirectory } ->
        packageDir </> name </> testDirectory <//> "*.hs"

sdist ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f FilePath
sdist = \case
  Package { name, version } -> do
    env <- ask
    pure
      ( getField @"buildDir" env
        </> getField @"packageDir" env
        </> name
        </> name
        <-> version
        <.> "tar.gz"
      )

test ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
test = \case
  Package { name, tests } -> traverse go tests
    where
    go = \case
      Test { suite } -> do
        env <- ask
        pure
          ( getField @"buildDir" env
            </> getField @"packageDir" env
            </> name
            </> "build"
            </> suite
            </> suite
            <.> "out"
          )

uploadToHackage ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f FilePath
uploadToHackage = \case
  Package { name, version } -> do
    env <- ask
    pure
      ( getField @"buildDir" env
        </> getField @"packageDir" env
        </> name
        </> name
        <-> version
      )

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
