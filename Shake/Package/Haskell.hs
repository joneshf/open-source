{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
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
  , packageType
  , sdist
  , test
  , uploadToHackage
  , writeDhall
  ) where

import "mtl" Control.Monad.Reader         (ReaderT, ask, asks)
import "text" Data.Text                   (pack, unpack)
import "shake" Development.Shake
    ( FilePattern
    , writeFileChanged
    , (<//>)
    )
import "shake" Development.Shake.FilePath (exe, (<.>), (</>))
import "dhall" Dhall
    ( Type(expected)
    , UnionType
    , constructor
    , field
    , list
    , record
    , string
    , union
    )
import "dhall" Dhall.Core                 (pretty)
import "base" GHC.Records                 (HasField(getField))

data Executable
  = Executable
    { executableName      :: String
    , executableDirectory :: FilePath
    }

data Manifest
  = Cabal
  | Hpack

data Package
  = Package
    { executables     :: [Executable]
    , manifest        :: Manifest
    , name            :: String
    , sourceDirectory :: FilePath
    , tests           :: [Test]
    , version         :: String
    }

data Test
  = Test
    { suite         :: String
    , testDirectory :: FilePath
    }

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

executableType :: Type Executable
executableType = record $ do
  executableName <- field (pack "executableName") string
  executableDirectory <- field (pack "executableDirectory") string
  pure Executable { executableName, executableDirectory }

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

manifestType :: Type Manifest
manifestType = union (cabal <> hpack)
  where
  cabal :: UnionType Manifest
  cabal = constructor (pack "Cabal") (record $ pure Cabal)
  hpack :: UnionType Manifest
  hpack = constructor (pack "Hpack") (record $ pure Hpack)

packageType :: Type Package
packageType = record $ do
  executables <- field (pack "executables") (list executableType)
  manifest <- field (pack "manifest") manifestType
  name <- field (pack "name") string
  sourceDirectory <- field (pack "sourceDirectory") string
  tests <- field (pack "tests") (list testType)
  version <- field (pack "version") string
  pure Package { executables, manifest, name, sourceDirectory, tests, version }

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

testType :: Type Test
testType = record $ do
  suite <- field (pack "suite") string
  testDirectory <- field (pack "testDirectory") string
  pure Test { suite, testDirectory }

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
    (unpack $ pretty $ expected executableType)
  writeFileChanged
    "Package/Haskell/Manifest.dhall"
    (unpack $ pretty $ expected manifestType)
  writeFileChanged
    "Package/Haskell/Package.dhall"
    (unpack $ pretty $ expected packageType)
  writeFileChanged
    "Package/Haskell/Test.dhall"
    (unpack $ pretty $ expected testType)
