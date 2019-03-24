{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.UploadToHackage (rules) where

import "mtl" Control.Monad.Reader         (ReaderT, asks, lift)
import "shake" Development.Shake
    ( CmdOption(Traced)
    , Exit(Exit)
    , Rules
    , Stdout(Stdout)
    , cmd
    , cmd_
    , need
    , writeFile'
    , (%>)
    )
import "shake" Development.Shake.FilePath (takeFileName, (<.>), (</>))
import "base" GHC.Records                 (HasField(getField))
import "base" System.Exit                 (ExitCode(ExitFailure, ExitSuccess))

(<->) :: FilePath -> FilePath -> FilePath
x <-> y = x <> "-" <> y

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  String ->
  ReaderT e Rules ()
rules name version = do
  buildDir <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  let build' = buildDir </> packageDir </> name

  lift $ build' </> name <-> version %> \out -> do
    (Exit x, Stdout result) <-
      cmd (Traced "cabal info") "cabal info" [takeFileName out]
    case x of
      ExitFailure _ -> do
        need
          [ "Shake/Haskell/UploadToHackage.hs"
          , build' </> ".build"
          , out <.> "tar.gz"
          ]
        cmd_ (Traced "cabal upload") "cabal upload" [out <.> "tar.gz"]
        writeFile' out ""
      ExitSuccess -> writeFile' out result
