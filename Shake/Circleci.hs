{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Circleci (rules) where

import "base" Control.Monad            (when)
import "base" Control.Monad.IO.Class   (liftIO)
import "mtl" Control.Monad.Reader      (ReaderT, asks, lift)
import "text" Data.Text                (pack)
import "text" Data.Text.Encoding       (decodeUtf8With)
import "text" Data.Text.Encoding.Error (lenientDecode)
import "shake" Development.Shake
    ( Rules
    , getDirectoryFiles
    , getHashedShakeVersion
    , need
    , writeFile'
    , (%>)
    )
import "base" GHC.Records              (HasField(getField))
import "this" Shake.Package            (Package)

import qualified "bytestring" Data.ByteString
import qualified "this" Shake.Package

rules ::
  ( HasField "packageDir" e FilePath
  , HasField "packages" e [Package]
  ) =>
  ReaderT e Rules ()
rules = do
  packageDir <- asks (getField @"packageDir")
  packages <- asks (getField @"packages")
  lift $ ".circleci/cache" %> \out -> do
    artifacts <-
      getDirectoryFiles
        ""
        ("Shake//*" : foldMap (Shake.Package.inputs packageDir) packages)
    need artifacts
    newHash <- liftIO (getHashedShakeVersion artifacts)
    oldHash <- liftIO (Data.ByteString.readFile out)
    writeFile' out newHash
    when (decodeUtf8With lenientDecode oldHash /= pack newHash) $
      fail
        ( unlines
          [ "The cache has changed."
          , "Please run `shake " <> out <> "` and commit the changes."
          ]
        )
