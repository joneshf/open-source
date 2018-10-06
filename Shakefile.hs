{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import "base" Control.Monad.IO.Class        (liftIO)
import "base" Data.Foldable                 (for_)
import "text" Data.Text                     (pack, unpack)
import "shake" Development.Shake
    ( CmdOption(Cwd, EchoStdout, FileStdout, Traced)
    , Exit(Exit)
    , Rules
    , ShakeOptions(shakeFiles, shakeThreads, shakeVersion)
    , Stdout(Stdout)
    , cmd
    , cmd_
    , getDirectoryFiles
    , getDirectoryFilesIO
    , getHashedShakeVersion
    , need
    , phony
    , removeFilesAfter
    , runAfter
    , shakeArgs
    , shakeOptions
    , want
    , writeFile'
    , writeFileChanged
    , (%>)
    )
import "shake" Development.Shake.FilePath
    ( dropExtension
    , replaceFileName
    , takeFileName
    , (<.>)
    , (</>)
    )
import "dhall" Dhall
    ( Interpret
    , auto
    , detailed
    , expected
    , input
    )
import "dhall" Dhall.Core                   (pretty)
import "base" GHC.Generics                  (Generic)
import "directory" System.Directory         (getCurrentDirectory)
import "base" System.Exit                   (ExitCode(ExitFailure, ExitSuccess))
import "typed-process" System.Process.Typed
    ( proc
    , runProcess_
    , setWorkingDir
    , shell
    )

data Package
  = Haskell
    { manifest :: Manifest
    , name     :: String
    , tests    :: [String]
    , version  :: String
    }
  deriving (Generic)

instance Interpret Package

data Manifest
  = Cabal
  | Hpack
  deriving (Generic)

instance Interpret Manifest

main :: IO ()
main = do
  writeFileChanged "Manifest.dhall" (unpack $ pretty $ expected $ auto @Manifest)
  writeFileChanged "Package.dhall" (unpack $ pretty $ expected $ auto @Package)
  packages' <- getDirectoryFilesIO "" ["packages/*/shake.dhall"]
  packages <- traverse (detailed . input auto . pack . ("./" <>)) packages'
  shakeVersion <- getHashedShakeVersion ["Shakefile.hs"]
  let options = shakeOptions { shakeFiles, shakeThreads, shakeVersion }
      shakeFiles = buildDir
      shakeThreads = 0
  shakeArgs options $ do
    want ["build"]

    phony "build" (need $ fmap build packages)

    phony "clean" (removeFilesAfter "" [buildDir])

    phony "sdist" (need $ fmap sdist packages)

    phony "shell" (runAfter $ runProcess_ $ shell "nix-shell --pure")

    phony "test" (need $ foldMap test packages)

    phony "upload-to-hackage" (need $ fmap uploadToHackage packages)

    buildDir </> ".update" %> \out ->
      cmd (FileStdout out) (Traced "cabal update") "cabal update"

    for_ packages $ \case
      Haskell manifest name tests version -> haskell manifest name tests version

(<->) :: FilePath -> FilePath -> FilePath
x <-> y = x <> "-" <> y

build :: Package ->  FilePath
build = \case
  Haskell { name } -> buildDir </> packageDir </> name </> ".build"

buildDir :: FilePath
buildDir = "_build"

ghciFlags :: [String]
ghciFlags =
  [ "-ferror-spans"
  , "-fno-break-on-exception"
  , "-fno-break-on-error"
  , "-fno-code"
  , "-j"
  , "-v1"
  ]

haskell :: Manifest -> String -> [String] -> String -> Rules ()
haskell manifest name tests version = do
  root <- liftIO getCurrentDirectory
  let build' = buildDir </> package
      package = packageDir </> name

  phony ("watch-" <> name) $ do
    need [build' </> ".configure"]
    runAfter
      ( runProcess_
      $ setWorkingDir package
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
    srcs <- getDirectoryFiles "" [package </> "src//*.hs"]
    need ((build' </> ".configure") : srcs)
    cmd
      (Cwd package)
      (FileStdout out)
      (Traced "cabal build")
      "cabal build"
      "--builddir"
      [root </> build']

  build' </> ".check" %> \out -> do
    need [package </> name <.> "cabal"]
    cmd
      (Cwd package)
      (EchoStdout True)
      (FileStdout out)
      (Traced "cabal check")
      "cabal check"

  build' </> ".configure" %> \out -> do
    need [buildDir </> ".update", package </> name <.> "cabal"]
    cmd
      (Cwd package)
      (FileStdout out)
      (Traced "cabal configure")
      "cabal configure"
      "--builddir"
      [root </> build']
      ("--enable-tests" <$ tests)

  for_ tests $ \suite -> do
    build' </> "build" </> suite </> suite %> \_ -> do
      srcs <-
        getDirectoryFiles
          ""
          [package </> "src//*.hs", package </> "test//*.hs"]
      need ((build' </> ".build") : srcs)
      cmd_
        (Cwd package)
        (Traced "cabal build")
        "cabal build"
        ["test:" <> suite]
        "--builddir"
        [root </> build']

    build' </> "build" </> suite </> suite <.> "out" %> \out -> do
      need [dropExtension out]
      cmd_
        (Cwd package)
        (FileStdout out)
        (Traced $ name <> " " <> suite)
        [((root </>) . dropExtension) out]

  build' </> name <-> version %> \out -> do
    (Exit x, Stdout result) <-
      cmd (Traced "cabal info") "cabal info" [takeFileName out]
    case x of
      ExitFailure _ -> do
        need [build' </> ".build", out <.> "tar.gz"]
        cmd_ (Traced "cabal upload") "cabal upload" [out <.> "tar.gz"]
        writeFile' out ""
      ExitSuccess -> writeFile' out result

  build' </> name <-> version <.> "tar.gz" %> \_ -> do
    srcs <- getDirectoryFiles "" [package </> "src//*.hs"]
    need ((build' </> ".check") : (build' </> ".configure") : srcs)
    cmd_
      (Cwd package)
      (Traced "cabal sdist")
      "cabal sdist"
      "--builddir"
      [root </> build']

  case manifest of
    Cabal -> mempty
    Hpack ->
      package </> name <.> "cabal" %> \out -> do
        need [replaceFileName out "package.yaml"]
        cmd_ (Cwd package) (Traced "hpack") "hpack"

packageDir :: FilePath
packageDir = "packages"

sdist :: Package -> FilePath
sdist = \case
  Haskell { name, version } ->
    buildDir </> packageDir </> name </> name <-> version <.> "tar.gz"

test :: Package -> [FilePath]
test = \case
  Haskell { name, tests } -> fmap go tests
    where
    go suite =
      buildDir </> packageDir </> name </> "build" </> suite </> suite <.> "out"

uploadToHackage :: Package -> FilePath
uploadToHackage = \case
  Haskell { name, version } ->
    buildDir </> packageDir </> name </> name <-> version
