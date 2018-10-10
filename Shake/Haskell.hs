{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Shake.Haskell (rules) where

import "base" Control.Monad.IO.Class        (liftIO)
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
import "this" Shake.Package
    ( Manifest(Cabal, Hpack)
    , Package(Haskell)
    , Test(Test, suite, testDirectory)
    )
import "directory" System.Directory         (getCurrentDirectory)
import "base" System.Exit                   (ExitCode(ExitFailure, ExitSuccess))
import "typed-process" System.Process.Typed (proc, runProcess_, setWorkingDir)

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
  FilePath ->
  FilePath ->
  Manifest ->
  String ->
  FilePath ->
  [Test] ->
  String ->
  Rules ()
package buildDir packageDir manifest name sourceDirectory tests version = do
  root <- liftIO getCurrentDirectory
  let build' = buildDir </> package'
      package' = packageDir </> name

  phony ("watch-" <> name) $ do
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

  build' </> ".build" %> \out -> do
    srcs <- getDirectoryFiles "" [package' </> sourceDirectory <//> "*.hs"]
    need ("Shake/Haskell.hs" : (build' </> ".configure") : srcs)
    cmd
      (Cwd package')
      (FileStdout out)
      (Traced "cabal build")
      "cabal build"
      "--builddir"
      [root </> build']

  build' </> ".configure" %> \out -> do
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

  for_ tests $ \case
    Test { testDirectory, suite } -> do
      build' </> "build" </> suite </> suite %> \_ -> do
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

      build' </> "build" </> suite </> suite <.> "out" %> \out -> do
        need ["Shake/Haskell.hs", dropExtension out]
        cmd_
          (Cwd package')
          (FileStdout out)
          (Traced $ name <> " " <> suite)
          [((root </>) . dropExtension) out]

  build' </> name <-> version %> \out -> do
    (Exit x, Stdout result) <-
      cmd (Traced "cabal info") "cabal info" [takeFileName out]
    case x of
      ExitFailure _ -> do
        need ["Shake/Haskell.hs", build' </> ".build", out <.> "tar.gz"]
        cmd_ (Traced "cabal upload") "cabal upload" [out <.> "tar.gz"]
        writeFile' out ""
      ExitSuccess -> writeFile' out result

  build' </> name <-> version <.> "tar.gz" %> \_ -> do
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
    Cabal -> mempty
    Hpack ->
      package' </> name <.> "cabal" %> \out -> do
        need ["Shake/Haskell.hs", replaceFileName out "package.yaml"]
        cmd_ (Cwd package') (Traced "hpack") "hpack"

rules :: FilePath -> FilePath -> [Package] -> Rules ()
rules buildDir packageDir packages = do
  buildDir <//> "*.hs.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".stylish-haskell.yaml", "Shake/Haskell.hs"]
    cmd_ (Traced "stylish-haskell") "stylish-haskell" "--inplace" [input]
    copyFileChanged input out
    needed ["Shake/Haskell.hs", input]

  buildDir <//> "*.hs.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".hlint.yaml", "Shake/Haskell.hs", input, out -<.> "format"]
    cmd_ (Traced "hlint") "hlint" [input]
    copyFileChanged input out

  for_ packages $ \case
    Haskell manifest name sourceDirectory tests' version ->
      package buildDir packageDir manifest name sourceDirectory tests' version
