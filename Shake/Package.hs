{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Package
  ( Package(..)
  , haskell
  , go
  , inputs
  , javaScript
  , packageType
  , rules
  , writeDhall
  ) where

import "base" Control.Applicative         (Alternative, empty)
import "base" Control.Monad.IO.Class      (liftIO)
import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "base" Data.Foldable               (fold)
import "base" Data.Maybe                  (catMaybes)
import "text" Data.Text                   (Text, pack, unpack)
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
import "dhall" Dhall                      (Type(Type, expected, extract))
import "dhall" Dhall.Core                 (Expr(..), pretty)
import "dhall" Dhall.Parser               (Src)
import "dhall" Dhall.TypeCheck            (X)
import "base" GHC.Exts                    (fromList)
import "base" GHC.Records                 (HasField(getField))

import qualified "this" Shake.Package.Go
import qualified "this" Shake.Package.Haskell
import qualified "this" Shake.Package.JavaScript

data Package
  = Go Shake.Package.Go.Package
  | Haskell Shake.Package.Haskell.Package
  | JavaScript Shake.Package.JavaScript.Package

binaries ::
  ( HasField "binDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
binaries = \case
  Go package -> Shake.Package.Go.binaries package
  Haskell package -> Shake.Package.Haskell.binaries package
  JavaScript package -> Shake.Package.JavaScript.binaries package

build ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f FilePath
build = \case
  Go package -> Shake.Package.Go.build package
  Haskell package -> Shake.Package.Haskell.build package
  JavaScript package -> Shake.Package.JavaScript.build package

executable ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
executable = \case
  Go _ -> pure mempty
  Haskell package -> Shake.Package.Haskell.executable package
  JavaScript _ -> pure mempty

go :: (Alternative f) => Package -> f Shake.Package.Go.Package
go = \case
  Go package -> pure package
  Haskell _ -> empty
  JavaScript _ -> empty

haskell :: (Alternative f) => Package -> f Shake.Package.Haskell.Package
haskell = \case
  Go _ -> empty
  Haskell package -> pure package
  JavaScript _ -> empty

inputs ::
  ( HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePattern]
inputs = \case
  Go package -> Shake.Package.Go.inputs package
  Haskell package -> Shake.Package.Haskell.inputs package
  JavaScript package -> Shake.Package.JavaScript.inputs package

javaScript :: (Alternative f) => Package -> f Shake.Package.JavaScript.Package
javaScript = \case
  Go _ -> empty
  Haskell _ -> empty
  JavaScript package -> pure package

packageType :: Type Package
packageType =
  union
    [ (pack "Go", fmap Go Shake.Package.Go.packageType)
    , (pack "Haskell", fmap Haskell Shake.Package.Haskell.packageType)
    , (pack "JavaScript", fmap JavaScript Shake.Package.JavaScript.packageType)
    ]

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
  sdistNeeds <- fmap catMaybes (traverse sdist packages)
  testNeeds <- fmap fold (traverse test packages)
  uploadToHackageNeeds <- fmap catMaybes (traverse uploadToHackage packages)

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
  ReaderT e f (Maybe FilePath)
sdist = \case
  Go _ -> pure mempty
  Haskell package -> fmap pure (Shake.Package.Haskell.sdist package)
  JavaScript _ -> pure mempty

test ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f [FilePath]
test = \case
  Go package -> Shake.Package.Go.test package
  Haskell package -> Shake.Package.Haskell.test package
  JavaScript _ -> pure mempty

union :: forall a. [(Text, Type a)] -> Type a
union xs = Type { expected, extract }
  where
  expected :: Expr Src X
  expected =
    Union (GHC.Exts.fromList $ (fmap . fmap) Dhall.expected xs)
  extract :: Expr Src X -> Maybe a
  extract = \case
    UnionLit alternate x _ -> do
      ty <- lookup alternate xs
      Dhall.extract ty x
    _ -> Nothing

uploadToHackage ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , Monad f
  ) =>
  Package ->
  ReaderT e f (Maybe FilePath)
uploadToHackage = \case
  Go _ -> pure mempty
  Haskell package -> fmap pure (Shake.Package.Haskell.uploadToHackage package)
  JavaScript _ -> pure mempty

writeDhall :: IO ()
writeDhall = do
  Shake.Package.Go.writeDhall
  Shake.Package.Haskell.writeDhall
  Shake.Package.JavaScript.writeDhall
  writeFileChanged "Package.dhall" (unpack $ pretty $ expected packageType)
