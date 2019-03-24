{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.JavaScript.PackageJSON (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "aeson" Data.Aeson                 (ToJSON)
import "aeson" Data.Aeson.Text            (encodeToLazyText)
import "text" Data.Text                   (Text, pack)
import "text" Data.Text.Lazy              (unpack)
import "shake" Development.Shake          (Rules, need, writeFileChanged, (%>))
import "shake" Development.Shake.FilePath ((</>))
import "base" GHC.Generics                (Generic)
import "base" GHC.Records                 (HasField(getField))
import "this" Shake.Package.JavaScript
    ( Dependency(Dependency, package, version)
    )

import qualified "unordered-containers" Data.HashMap.Strict

data PackageJSON
  = PackageJSON
    { dependencies :: Data.HashMap.Strict.HashMap Text Text
    , license      :: Text
    , name         :: Text
    , version      :: Text
    }
  deriving (Generic)

instance ToJSON PackageJSON

rules ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  [Dependency] ->
  String ->
  String ->
  String ->
  ReaderT e Rules ()
rules dependencies license name version' = do
  buildDir' <- asks (getField @"buildDir")
  packageDir' <- asks (getField @"packageDir")

  let buildDir = buildDir' </> packageDir
      dependency = \case
        Dependency { package, version } -> (pack package, pack version)
      packageJSON =
        PackageJSON
          { dependencies =
            Data.HashMap.Strict.fromList (fmap dependency dependencies)
          , license = pack license
          , name = pack name
          , version = pack version'
          }
      packageDir = packageDir' </> name

  lift $ buildDir </> "package.json" %> \out -> do
    need ["Shake/JavaScript/PackageJSON.hs", packageDir </> "shake.dhall"]
    writeFileChanged out (unpack $ encodeToLazyText packageJSON)
