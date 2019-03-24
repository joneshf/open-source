{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.JavaScript (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "aeson" Data.Aeson                 (ToJSON)
import "aeson" Data.Aeson.Text            (encodeToLazyText)
import "base" Data.Foldable               (for_)
import "text" Data.Text                   (Text, pack)
import "text" Data.Text.Lazy              (unpack)
import "shake" Development.Shake
    ( CmdOption(Cwd, FileStdout, Traced)
    , Rules
    , cmd_
    , copyFileChanged
    , getDirectoryFiles
    , need
    , produces
    , writeFile'
    , writeFileChanged
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath (takeDirectory, takeFileName, (</>))
import "base" GHC.Generics                (Generic)
import "base" GHC.Records                 (HasField(getField))
import "this" Shake.Package.JavaScript
    ( Bin(Bin, file, name, nodeVersion)
    , Dependency(Dependency, package, version)
    , Package(Package, bin, dependencies, license, name, sourceDirectory, version)
    )

import qualified "unordered-containers" Data.HashMap.Strict
import qualified "this" Shake.JavaScript.Format
import qualified "this" Shake.JavaScript.Lint
import qualified "this" Shake.Package

data PackageJSON
  = PackageJSON
    { dependencies :: Data.HashMap.Strict.HashMap Text Text
    , license      :: Text
    , name         :: Text
    , version      :: Text
    }
  deriving (Generic)

instance ToJSON PackageJSON

package ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  Package ->
  ReaderT e Rules ()
package = \case
  Package
    { bin = bins
    , dependencies
    , license = packageLicense
    , name = packageName
    , sourceDirectory
    , version = packageVersion
    } -> do
      binDir <- asks (getField @"binDir")
      buildDir <- asks (getField @"buildDir")
      packageDir' <- asks (getField @"packageDir")

      let buildPackageDir = buildDir </> packageDir
          packageDir = packageDir' </> packageName

      lift $ buildPackageDir </> ".build" %> \out -> do
        need ["Shake/JavaScript.hs", buildPackageDir </> "package.json"]
        cmd_
          (Cwd buildPackageDir)
          (FileStdout out)
          (Traced "yarn install")
          "yarn install"
        produces [buildPackageDir </> "yarn.lock"]

      lift $ buildPackageDir </> ".copied" %> \out -> do
        srcs <- getDirectoryFiles "" [packageDir </> sourceDirectory <//> "*.js"]
        need ("Shake/JavaScript.hs" : fmap (buildDir </>) srcs)
        writeFile' out ""

      lift $ buildPackageDir </> "package.json" %> \out -> do
        let dependency = \case
              Dependency { package = package', version } ->
                (pack package', pack version)
            packageJSON =
              PackageJSON
                { dependencies =
                  Data.HashMap.Strict.fromList (fmap dependency dependencies)
                , license = pack packageLicense
                , name = pack packageName
                , version = pack packageVersion
                }
        need ["Shake/JavaScript.hs", packageDir </> "shake.dhall"]
        writeFileChanged out (unpack $ encodeToLazyText packageJSON)

      lift $ buildPackageDir </> sourceDirectory <//> "*.js" %> \out -> do
        let src = drop (length buildDir + 1) out
        need [src]
        copyFileChanged src out

      for_ bins $ \case
        Bin { file, name, nodeVersion } -> do
          lift $ binDir </> name </> "*/*" %> \out -> do
            let binary =
                  buildPackageDir </> "bin" </> target </> takeFileName out
                target = (takeFileName . takeDirectory) out
            need ["Shake/JavaScript.hs", binary]
            copyFileChanged binary out

          lift $ buildPackageDir </> file %> \out -> do
            need ["Shake/JavaScript.hs", packageDir </> file]
            copyFileChanged (packageDir </> file) out

          lift $ buildPackageDir </> "bin" </> "*/*" %> \out -> do
            let fileName = takeFileName out
                target = (takeFileName . takeDirectory) out
            need
              [ "Shake/JavaScript.hs"
              , buildPackageDir </> ".build"
              , buildPackageDir </> ".copied"
              , buildPackageDir </> file
              ]

            cmd_
              (Cwd buildPackageDir)
              (Traced "nexe")
              "yarn run nexe"
              "--input"
              [file]
              "--output"
              ["bin" </> target </> fileName]
              "--target"
              [target <> "-x64-" <> nodeVersion]

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , HasField "packages" e [Shake.Package.Package]
  ) =>
  ReaderT e Rules ()
rules = do
  packages <- asks (getField @"packages")

  Shake.JavaScript.Format.rules

  Shake.JavaScript.Lint.rules

  for_ packages $ \case
    Shake.Package.Haskell _ -> pure mempty
    Shake.Package.JavaScript x -> Shake.JavaScript.package x
