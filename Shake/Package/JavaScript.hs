{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Package.JavaScript
  ( Bin(..)
  , Dependency(..)
  , Package(..)
  , binaries
  , build
  , inputs
  , packageType
  , writeDhall
  ) where

import "mtl" Control.Monad.Reader         (ReaderT, ask, asks)
import "base" Data.Foldable               (fold)
import "text" Data.Text                   (pack, unpack)
import "shake" Development.Shake          (writeFileChanged, (<//>))
import "shake" Development.Shake.FilePath ((<.>), (</>))
import "dhall" Dhall
    ( Type
    , expected
    , field
    , list
    , record
    , string
    )
import "dhall" Dhall.Core                 (pretty)
import "base" GHC.Records                 (HasField(getField))

data Bin
  = Bin
    { file        :: FilePath
    , name        :: String
    , nodeVersion :: String
    }

data Dependency
  = Dependency
    { package :: String
    , version :: String
    }

data Package
  = Package
    { bin             :: [Bin]
    , dependencies    :: [Dependency]
    , license         :: String
    , name            :: String
    , sourceDirectory :: FilePath
    , version         :: String
    }

binType :: Type Bin
binType = record $ do
  file <- field (pack "file") string
  name <- field (pack "name") string
  nodeVersion <- field (pack "node-version") string
  pure Bin { file, name, nodeVersion }

binaries ::
  ( HasField "binDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
binaries = \case
  Package { bin } -> fmap fold (traverse go bin)
    where
    go = \case
      Bin { name } -> do
        env <- ask
        pure
          [ getField @"binDir" env </> name </> "linux" </> name
          , getField @"binDir" env </> name </> "mac" </> name
          , getField @"binDir" env </> name </> "win" </> name <.> "exe"
          ]

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

dependencyType :: Type Dependency
dependencyType = record $ do
  package <- field (pack "package") string
  version <- field (pack "version") string
  pure Dependency { package, version }

inputs ::
  ( HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
inputs = \case
  Package { name, sourceDirectory } -> do
    packageDir' <- asks (getField @"packageDir")
    let packageDir = packageDir' </> name
    pure
      [ packageDir </> "shake.dhall"
      , packageDir </> "package.json"
      , packageDir </> sourceDirectory <//> "*.js"
      ]

packageType :: Type Package
packageType = record $ do
  bin <- field (pack "bin") (list binType)
  dependencies <- field (pack "dependencies") (list dependencyType)
  license <- field (pack "license") string
  name <- field (pack "name") string
  sourceDirectory <- field (pack "sourceDirectory") string
  version <- field (pack "version") string
  pure Package { bin, dependencies, license, name, sourceDirectory, version }

writeDhall :: IO ()
writeDhall = do
  writeFileChanged
    "Package/JavaScript/Bin.dhall"
    (unpack $ pretty $ expected binType)
  writeFileChanged
    "Package/JavaScript/Package.dhall"
    (unpack $ pretty $ expected packageType)
