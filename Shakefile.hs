{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import "base" Control.Monad                 (when)
import "base" Control.Monad.IO.Class        (liftIO)
import "base" Data.Foldable                 (for_)
import "base" Data.List.NonEmpty            (nonEmpty)
import "text" Data.Text                     (pack, unpack)
import "text" Data.Text.Encoding            (decodeUtf8With)
import "text" Data.Text.Encoding.Error      (lenientDecode)
import "shake" Development.Shake
    ( Change(ChangeModtimeAndDigest)
    , CmdOption(Cwd, EchoStdout, FileStdout, Traced)
    , Exit(Exit)
    , FilePattern
    , Rules
    , ShakeOptions(shakeChange, shakeFiles, shakeThreads, shakeVersion)
    , Stdout(Stdout)
    , cmd
    , cmd_
    , copyFileChanged
    , getDirectoryFiles
    , getDirectoryFilesIO
    , getHashedShakeVersion
    , need
    , needed
    , phony
    , removeFilesAfter
    , runAfter
    , shakeArgs
    , shakeOptions
    , want
    , writeFile'
    , writeFileChanged
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath
    ( dropDirectory1
    , dropExtension
    , replaceFileName
    , takeDirectory
    , takeFileName
    , (-<.>)
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

import qualified "bytestring" Data.ByteString

data Manifest
  = Cabal
  | Hpack
  deriving (Generic)

instance Interpret Manifest

data Package
  = Haskell
    { manifest        :: Manifest
    , name            :: String
    , sourceDirectory :: FilePath
    , tests           :: [Test]
    , version         :: String
    }
  deriving (Generic)

instance Interpret Package

data Test
  = Test
    { suite         :: String
    , testDirectory :: FilePath
    }
  deriving (Generic)

instance Interpret Test

main :: IO ()
main = do
  writeFileChanged "Manifest.dhall" (unpack $ pretty $ expected $ auto @Manifest)
  writeFileChanged "Package.dhall" (unpack $ pretty $ expected $ auto @Package)
  writeFileChanged "Test.dhall" (unpack $ pretty $ expected $ auto @Test)
  packages' <- getDirectoryFilesIO "" ["packages/*/shake.dhall"]
  packages <- traverse (detailed . input auto . pack . ("./" <>)) packages'
  shakeVersion <- getHashedShakeVersion ["Shakefile.hs"]
  inputFiles <- getDirectoryFilesIO "" (foldMap inputs packages)
  let allFiles = "Shakefile.hs" : inputFiles
      buildNeeds = fmap build packages
      ciNeeds =
        buildNeeds <> formatNeeds <> lintNeeds <> sdistNeeds <> testNeeds
      formatNeeds = fmap (\x -> buildDir </> x <.> "format") allFiles
      lintNeeds = fmap (\x -> buildDir </> x <.> "lint") allFiles
      options = shakeOptions
        { shakeChange = ChangeModtimeAndDigest
        , shakeFiles = buildDir
        , shakeThreads = 0
        , shakeVersion
        }
      sdistNeeds = fmap sdist packages
      testNeeds = foldMap test packages
  shakeArgs options $ do
    want ["build"]

    phony "build" (need buildNeeds)

    phony "ci" (need ciNeeds)

    phony "clean" (removeFilesAfter "" [buildDir])

    phony "format" (need formatNeeds)

    phony "lint" (need lintNeeds)

    phony "sdist" (need sdistNeeds)

    phony "shell" (runAfter $ runProcess_ $ shell "nix-shell --pure")

    phony "test" (need testNeeds)

    phony "upload-to-hackage" (need $ fmap uploadToHackage packages)

    buildDir </> ".update" %> \out ->
      cmd (FileStdout out) (Traced "cabal update") "cabal update"

    buildDir <//> "*.cabal.format" %> \out ->
      -- Skip over formatting cabal files.
      -- Although `cabal format` does what it says it will,
      -- it's riddled with bugs.
      -- Most annoyingly, it drops comments.
      -- Once that's fixed, use it.
      copyFileChanged ((dropDirectory1 . dropExtension) out) out

    buildDir <//> "*.cabal.lint" %> \out -> do
      let input' = (dropDirectory1 . dropExtension) out
      need [input', out -<.> "format"]
      cmd_
        (Cwd $ takeDirectory input')
        (EchoStdout True)
        (FileStdout out)
        (Traced "cabal check")
        "cabal check"

    buildDir <//> "*.dhall.format" %> \out -> do
      let input' = (dropDirectory1 . dropExtension) out
      need [out -<.> "lint"]
      cmd_ (Traced "dhall format") "dhall format" "--inplace" [input']
      copyFileChanged input' out
      needed [input']

    buildDir <//> "*.dhall.lint" %> \out -> do
      let input' = (dropDirectory1 . dropExtension) out
      cmd_ (Traced "dhall lint") "dhall lint" "--inplace" [input']
      copyFileChanged input' out
      needed [input']

    buildDir <//> "*.hs.format" %> \out -> do
      let input' = (dropDirectory1 . dropExtension) out
      need [".stylish-haskell.yaml"]
      cmd_ (Traced "stylish-haskell") "stylish-haskell" "--inplace" [input']
      copyFileChanged input' out
      needed [input']

    buildDir <//> "*.hs.lint" %> \out -> do
      let input' = (dropDirectory1 . dropExtension) out
      need [".hlint.yaml", input', out -<.> "format"]
      cmd_ (Traced "hlint") "hlint" [input']
      copyFileChanged input' out

    buildDir <//> "*.yaml.format" %> \out -> do
      let input' = (dropDirectory1 . dropExtension) out
      need [".prettierrc"]
      cmd_ (Traced "prettier") "prettier" "--write" [input']
      copyFileChanged input' out
      needed [input']

    buildDir <//> "*.yaml.lint" %> \out -> do
      let input' = (dropDirectory1 . dropExtension) out
      need [".yamllint", input', out -<.> "format"]
      cmd_ (Traced "yamllint") "yamllint" "--strict" [input']
      copyFileChanged input' out

    ".circleci/cache" %> \out -> do
      artifacts <-
        getDirectoryFiles "" ("Shakefile.hs" : foldMap inputs packages)
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

    for_ packages $ \case
      Haskell manifest name sourceDirectory tests' version ->
        haskell manifest name sourceDirectory tests' version

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

haskell :: Manifest -> String -> FilePath -> [Test] -> String -> Rules ()
haskell manifest name sourceDirectory tests version = do
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
    srcs <- getDirectoryFiles "" [package </> sourceDirectory <//> "*.hs"]
    need ((build' </> ".configure") : srcs)
    cmd
      (Cwd package)
      (FileStdout out)
      (Traced "cabal build")
      "cabal build"
      "--builddir"
      [root </> build']

  build' </> ".configure" %> \out -> do
    need [buildDir </> ".update", package </> name <.> "cabal"]
    cmd
      (Cwd package)
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
            [ package </> sourceDirectory <//> "*.hs"
            , package </> testDirectory </> suite <//> "*.hs"
            ]
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
    srcs <- getDirectoryFiles "" [package </> sourceDirectory <//> "*.hs"]
    need ((build' </> name <.> "cabal.lint") : (build' </> ".configure") : srcs)
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

inputs :: Package -> [FilePattern]
inputs = \case
  Haskell { manifest, name, sourceDirectory, tests } ->
    config : manifestInput manifest : sourceInput : fmap testInput tests
    where
    config = "packages" </> name </> "shake.dhall"
    manifestInput = \case
      Cabal -> "packages" </> name </> name <.> "cabal"
      Hpack -> "packages" </> name </> "package.yaml"
    sourceInput = "packages" </> name </> sourceDirectory <//> "*.hs"
    testInput = \case
      Test { testDirectory } ->
        "packages" </> name </> testDirectory <//> "*.hs"

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
    go = \case
      Test { suite } ->
        buildDir
          </> packageDir
          </> name
          </> "build"
          </> suite
          </> suite
          <.> "out"

uploadToHackage :: Package -> FilePath
uploadToHackage = \case
  Haskell { name, version } ->
    buildDir </> packageDir </> name </> name <-> version
