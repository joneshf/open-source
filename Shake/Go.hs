{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Go (rules) where

import "mtl" Control.Monad.Reader (ReaderT, asks)
import "base" Data.Foldable       (for_)
import "shake" Development.Shake  (Rules)
import "base" GHC.Records         (HasField(getField))
import "this" Shake.Package.Go    (Package(Package))

import qualified "this" Shake.Go.Bin
import qualified "this" Shake.Go.Build
import qualified "this" Shake.Go.Format
import qualified "this" Shake.Go.Lint
import qualified "this" Shake.Go.Test

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , HasField "packages" e [Package]
  ) =>
  ReaderT e Rules ()
rules = do
  packages <- asks (getField @"packages")

  Shake.Go.Format.rules

  Shake.Go.Lint.rules

  for_ packages $ \case
    Package bins name sourceDirectory _version -> do
      Shake.Go.Build.rules name sourceDirectory
      Shake.Go.Test.rules name sourceDirectory
      for_ bins (Shake.Go.Bin.rules name sourceDirectory)
