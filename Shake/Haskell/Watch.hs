{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell.Watch (rules) where

import "base" Control.Monad.IO.Class        (liftIO)
import "mtl" Control.Monad.Reader           (ReaderT, asks, lift)
import "shake" Development.Shake            (Rules, need, phony, runAfter)
import "shake" Development.Shake.FilePath   ((</>))
import "base" GHC.Records                   (HasField(getField))
import "directory" System.Directory         (getCurrentDirectory)
import "typed-process" System.Process.Typed (proc, runProcess_, setWorkingDir)

ghciFlags :: [String]
ghciFlags =
  [ "-ferror-spans"
  , "-fno-break-on-exception"
  , "-fno-break-on-error"
  , "-fno-code"
  , "-j"
  , "-v1"
  ]

rules ::
  ( HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  String ->
  ReaderT e Rules ()
rules name = do
  buildDir <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  root <- liftIO getCurrentDirectory
  let build' = buildDir </> package'
      package' = packageDir </> name

  lift $ phony ("watch-" <> name) $ do
    need ["Shake/Haskell/Watch.hs", build' </> ".configure"]
    runAfter
      ( runProcess_
      $ setWorkingDir package'
      $ proc
        "ghcid"
        [ "--command"
        , "cabal repl lib:"
          <> name
          <> " --builddir "
          <> root </> build'
          <> " --ghc-options '"
          <> unwords ghciFlags
          <> "'"
        ]
      )
