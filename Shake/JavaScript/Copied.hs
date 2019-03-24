{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.JavaScript.Copied (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( Rules
    , copyFileChanged
    , getDirectoryFiles
    , need
    , writeFile'
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath ((</>))
import "base" GHC.Records                 (HasField(getField))

rules ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  FilePath ->
  ReaderT e Rules ()
rules name sourceDirectory = do
  buildDir' <- asks (getField @"buildDir")
  packageDir' <- asks (getField @"packageDir")

  let buildDir = buildDir' </> packageDir
      packageDir = packageDir' </> name

  lift $ buildDir </> ".copied" %> \out -> do
    srcs <- getDirectoryFiles "" [packageDir </> sourceDirectory <//> "*.js"]
    need ("Shake/JavaScript/Copied.hs" : fmap (buildDir </>) srcs)
    writeFile' out ""

  lift $ buildDir </> sourceDirectory <//> "*.js" %> \out -> do
    let src = drop (length buildDir + 1) out
    need ["Shake/JavaScript/Copied.hs", src]
    copyFileChanged src out
