{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Package.Go
  ( Bin(..)
  , Package(..)
  , binaries
  , build
  , inputs
  , packageType
  , test
  , writeDhall
  ) where

import "mtl" Control.Monad.Reader         (ReaderT, ask, asks)
import "base" Data.Foldable               (fold)
import "text" Data.Text                   (pack, unpack)
import "shake" Development.Shake          (writeFileChanged)
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
    { file :: FilePath
    , name :: String
    }

data Package
  = Package
    { bin             :: [Bin]
    , name            :: String
    , sourceDirectory :: FilePath
    , version         :: String
    }

binType :: Type Bin
binType = record $ do
  file <- field (pack "file") string
  name <- field (pack "name") string
  pure Bin { file, name }

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
          , getField @"binDir" env </> name </> "darwin" </> name
          , getField @"binDir" env </> name </> "windows" </> name <.> "exe"
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
      , packageDir </> sourceDirectory </> "*.go"
      ]

packageType :: Type Package
packageType = record $ do
  bin <- field (pack "bin") (list binType)
  name <- field (pack "name") string
  sourceDirectory <- field (pack "sourceDirectory") string
  version <- field (pack "version") string
  pure Package { bin, name, sourceDirectory, version }

test ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
test = \case
  Package { name } -> do
    env <- ask
    pure
      [ getField @"buildDir" env
        </> getField @"packageDir" env
        </> name
        </> ".test"
      ]

writeDhall :: IO ()
writeDhall = do
  writeFileChanged
    "Package/Go/Bin.dhall"
    (unpack $ pretty $ expected binType)
  writeFileChanged
    "Package/Go/Package.dhall"
    (unpack $ pretty $ expected packageType)
