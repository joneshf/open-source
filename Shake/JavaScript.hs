{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.JavaScript (rules) where

import "mtl" Control.Monad.Reader      (ReaderT, asks)
import "base" Data.Foldable            (for_)
import "shake" Development.Shake       (Rules)
import "base" GHC.Records              (HasField(getField))
import "this" Shake.Package.JavaScript (Package(Package))

import qualified "this" Shake.JavaScript.Bin
import qualified "this" Shake.JavaScript.Build
import qualified "this" Shake.JavaScript.Copied
import qualified "this" Shake.JavaScript.Format
import qualified "this" Shake.JavaScript.Lint
import qualified "this" Shake.JavaScript.PackageJSON

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , HasField "packages" e [Package]
  ) =>
  ReaderT e Rules ()
rules = do
  packages <- asks (getField @"packages")

  Shake.JavaScript.Format.rules

  Shake.JavaScript.Lint.rules

  for_ packages $ \case
    (Package bins dependencies license name sourceDirectory version) -> do
      Shake.JavaScript.Build.rules name
      Shake.JavaScript.Copied.rules name sourceDirectory
      Shake.JavaScript.PackageJSON.rules dependencies license name version
      for_ bins (Shake.JavaScript.Bin.rules name)
