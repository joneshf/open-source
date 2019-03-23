{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Shake.Haskell (rules) where

import "base" Control.Monad.IO.Class        (liftIO)
import "mtl" Control.Monad.Reader           (ReaderT, asks, lift)
import "base" Data.Foldable                 (for_)
import "base" Data.List.NonEmpty            (nonEmpty)
import "shake" Development.Shake
    ( CmdOption(Cwd, FileStdout, Traced)
    , Exit(Exit)
    , Rules
    , Stdout(Stdout)
    , cmd
    , cmd_
    , copyFileChanged
    , getDirectoryFiles
    , need
    , needed
    , phony
    , runAfter
    , writeFile'
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath
    ( dropDirectory1
    , dropExtension
    , replaceFileName
    , takeFileName
    , (-<.>)
    , (<.>)
    , (</>)
    )
import "base" GHC.Records                   (HasField(getField))
import "this" Shake.Package.Haskell
    ( Executable(Executable, executableDirectory, executableName)
    , Manifest(Cabal, Hpack)
    , Package(Package)
    , Test(Test, suite, testDirectory)
    )
import "directory" System.Directory         (getCurrentDirectory)
import "base" System.Exit                   (ExitCode(ExitFailure, ExitSuccess))
import "typed-process" System.Process.Typed (proc, runProcess_, setWorkingDir)

import qualified "this" Shake.Package

(<->) :: FilePath -> FilePath -> FilePath
x <-> y = x <> "-" <> y

ghciFlags :: [String]
ghciFlags =
  [ "-ferror-spans"
  , "-fno-break-on-exception"
  , "-fno-break-on-error"
  , "-fno-code"
  , "-j"
  , "-v1"
  ]

package ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  ) =>
  [Executable] ->
  Manifest ->
  String ->
  FilePath ->
  [Test] ->
  String ->
  ReaderT e Rules ()
package exes manifest name sourceDirectory tests version = do
  binDir <- asks (getField @"binDir")
  buildDir <- asks (getField @"buildDir")
  packageDir <- asks (getField @"packageDir")
  root <- liftIO getCurrentDirectory
  let build' = buildDir </> package'
      package' = packageDir </> name

  lift $ phony ("watch-" <> name) $ do
    need ["Shake/Haskell.hs", build' </> ".configure"]
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

  lift $ build' </> ".build" %> \out -> do
    srcs <- getDirectoryFiles "" [package' </> sourceDirectory <//> "*.hs"]
    need ("Shake/Haskell.hs" : (build' </> ".configure") : srcs)
    cmd
      (Cwd package')
      (FileStdout out)
      (Traced "cabal build")
      "cabal build"
      "--builddir"
      [root </> build']

  lift $ build' </> ".configure" %> \out -> do
    need
      [ "Shake/Haskell.hs"
      , buildDir </> ".update"
      , package' </> name <.> "cabal"
      ]
    cmd
      (Cwd package')
      (FileStdout out)
      (Traced "cabal configure")
      "cabal configure"
      "--builddir"
      [root </> build']
      ("--enable-tests" <$ nonEmpty tests)

  for_ exes $ \case
    Executable { executableDirectory, executableName } -> do
      lift $ binDir </> executableName %> \out -> do
        let binary = build' </> "build" </> executableName </> executableName
        need ["Shake/Haskell.hs", binary]
        copyFileChanged binary out

      lift $ build' </> "build" </> executableName </> executableName %> \_ -> do
        srcs <-
          getDirectoryFiles
            ""
            [ package' </> sourceDirectory <//> "*.hs"
            , package' </> executableDirectory <//> "*.hs"
            ]
        need ("Shake/Haskell.hs" : (build' </> ".build") : srcs)
        cmd_
          (Cwd package')
          (Traced "cabal build")
          "cabal build"
          ["exe:" <> executableName]
          "--builddir"
          [root </> build']

  for_ tests $ \case
    Test { testDirectory, suite } -> do
      lift $ build' </> "build" </> suite </> suite %> \_ -> do
        srcs <-
          getDirectoryFiles
            ""
            [ package' </> sourceDirectory <//> "*.hs"
            , package' </> testDirectory </> suite <//> "*.hs"
            ]
        need ("Shake/Haskell.hs" : (build' </> ".build") : srcs)
        cmd_
          (Cwd package')
          (Traced "cabal build")
          "cabal build"
          ["test:" <> suite]
          "--builddir"
          [root </> build']

      lift $ build' </> "build" </> suite </> suite <.> "out" %> \out -> do
        need ["Shake/Haskell.hs", dropExtension out]
        cmd_
          (Cwd package')
          (FileStdout out)
          (Traced $ name <> " " <> suite)
          [((root </>) . dropExtension) out]

  lift $ build' </> name <-> version %> \out -> do
    (Exit x, Stdout result) <-
      cmd (Traced "cabal info") "cabal info" [takeFileName out]
    case x of
      ExitFailure _ -> do
        need ["Shake/Haskell.hs", build' </> ".build", out <.> "tar.gz"]
        cmd_ (Traced "cabal upload") "cabal upload" [out <.> "tar.gz"]
        writeFile' out ""
      ExitSuccess -> writeFile' out result

  lift $ build' </> name <-> version <.> "tar.gz" %> \_ -> do
    srcs <- getDirectoryFiles "" [package' </> sourceDirectory <//> "*.hs"]
    need
      ( "Shake/Haskell.hs"
      : (build' </> name <.> "cabal.lint")
      : (build' </> ".configure")
      : srcs
      )
    cmd_
      (Cwd package')
      (Traced "cabal sdist")
      "cabal sdist"
      "--builddir"
      [root </> build']

  case manifest of
    Cabal -> lift mempty
    Hpack ->
      lift $ package' </> name <.> "cabal" %> \out -> do
        need ["Shake/Haskell.hs", replaceFileName out "package.yaml"]
        cmd_ (Cwd package') (Traced "hpack") "hpack"

rules ::
  ( HasField "binDir" e FilePath
  , HasField "buildDir" e FilePath
  , HasField "packageDir" e FilePath
  , HasField "packages" e [Shake.Package.Package]
  ) =>
  ReaderT e Rules ()
rules = do
  buildDir <- asks (getField @"buildDir")
  packages <- asks (getField @"packages")
  lift $ buildDir <//> "*.hs.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".stylish-haskell.yaml"]
    cmd_ (Traced "stylish-haskell") "stylish-haskell" "--inplace" [input]
    copyFileChanged input out
    needed ["Shake/Haskell.hs", input]

  lift $ buildDir <//> "*.hs.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".hlint.yaml", "Shake/Haskell.hs", input, out -<.> "format"]
    cmd_ (Traced "hlint") "hlint" [input]
    copyFileChanged input out

  for_ packages $ \case
    Shake.Package.Haskell
      (Package exes manifest name sourceDirectory tests' version) ->
        package exes manifest name sourceDirectory tests' version
    Shake.Package.JavaScript _ -> pure mempty
