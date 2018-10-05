{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Main where

import "shake" Development.Shake
    ( CmdOption(Cwd, EchoStdout, FileStdout, Traced)
    , Exit(Exit)
    , ShakeOptions(shakeFiles, shakeThreads, shakeVersion)
    , Stdout(Stdout)
    , cmd
    , cmd_
    , getDirectoryFiles
    , getHashedShakeVersion
    , need
    , phony
    , removeFilesAfter
    , runAfter
    , shakeArgs
    , shakeOptions
    , want
    , writeFile'
    , (%>)
    )
import "shake" Development.Shake.FilePath
    ( dropDirectory1
    , dropExtension
    , replaceFileName
    , takeFileName
    , (<.>)
    , (</>)
    )
import "base" System.Exit                   (ExitCode(ExitFailure, ExitSuccess))
import "typed-process" System.Process.Typed (runProcess_, setWorkingDir, shell, proc)

main :: IO ()
main = do
  shakeVersion <- getHashedShakeVersion ["Shakefile.hs"]
  let options = shakeOptions { shakeFiles, shakeThreads, shakeVersion }
      shakeFiles = buildDir
      shakeThreads = 0
  shakeArgs options $ do
    want ["build"]

    phony "build" (need [buildRollbarHS </> ".build"])

    phony "clean" (removeFilesAfter "" [buildDir, distRollbarHS])

    phony "sdist"
      ( need
        [distRollbarHS </> rollbarHS <> "-" <> rollbarHSVersion <> ".tar.gz"]
      )

    phony "shell" (runAfter $ runProcess_ $ shell "nix-shell --pure")

    phony "test" (need [distRollbarHS </> "build/doc-test/doc-test.out"])

    phony "upload-to-hackage"
      (need [distRollbarHS </> rollbarHS <> "-" <> rollbarHSVersion])

    phony ("watch-" <> rollbarHS) $ do
      need [buildRollbarHS </> ".configure"]
      runAfter
        ( runProcess_
        $ setWorkingDir packageRollbarHS
        $ proc
          "ghcid"
          [ "--command"
          , "cabal repl lib:"
            <> rollbarHS
            <> " --ghc-options '"
            <> unwords ghciFlags
            <> "'"
          ]
        )

    buildDir </> ".update" %> \out ->
      cmd (FileStdout out) (Traced "cabal update") "cabal update"

    buildRollbarHS </> ".build" %> \out -> do
      srcs <- getDirectoryFiles "" [packageRollbarHS </> "src//*.hs"]
      need ((buildRollbarHS </> ".configure") : srcs)
      cmd
        (Cwd packageRollbarHS)
        (FileStdout out)
        (Traced "cabal build")
        "cabal build"

    buildRollbarHS </> ".check" %> \out -> do
      need [packageRollbarHS </> rollbarHS <.> "cabal"]
      cmd
        (Cwd packageRollbarHS)
        (EchoStdout True)
        (FileStdout out)
        (Traced "cabal check")
        "cabal check"

    buildRollbarHS </> ".configure" %> \out -> do
      need [buildDir </> ".update", packageRollbarHS </> rollbarHS <.> "cabal"]
      cmd
        (Cwd packageRollbarHS)
        (FileStdout out)
        (Traced "cabal configure")
        "cabal configure"
        "--enable-tests"

    distRollbarHS </> "build/doc-test/doc-test" %> \_ -> do
      srcs <-
        getDirectoryFiles
          ""
          [packageRollbarHS </> "src//*.hs", packageRollbarHS </> "test//*.hs"]
      need ((buildRollbarHS </> ".build") : srcs)
      cmd_
        (Cwd packageRollbarHS)
        (Traced "cabal build")
        "cabal build"
        "test:doc-test"

    distRollbarHS </> "build/doc-test/doc-test.out" %> \out -> do
      need [dropExtension out]
      cmd_
        (Cwd packageRollbarHS)
        (FileStdout out)
        (Traced "rollbar doc-test")
        [(dropDirectory1 . dropDirectory1 . dropExtension) out]

    distRollbarHS </> rollbarHS <> "-" <> rollbarHSVersion <.> "tar.gz" %> \_ -> do
      srcs <- getDirectoryFiles "" [packageRollbarHS </> "src//*.hs"]
      need
        ( (buildRollbarHS </> ".check")
        : (buildRollbarHS </> ".configure")
        : srcs
        )
      cmd_ (Cwd packageRollbarHS) (Traced "cabal sdist") "cabal sdist"

    distRollbarHS </> rollbarHS <> "-" <> rollbarHSVersion %> \out -> do
      (Exit x, Stdout result) <-
        cmd (Traced "cabal info") "cabal info" [takeFileName out]
      case x of
        ExitFailure _ -> do
          need [buildRollbarHS </> ".build", out <.> "tar.gz"]
          cmd_ (Traced "cabal upload") "cabal upload" [out <.> "tar.gz"]
          writeFile' out ""
        ExitSuccess -> writeFile' out result

    packageRollbarHS </> rollbarHS <.> "cabal" %> \out -> do
      need [replaceFileName out "package.yaml"]
      cmd_ (Cwd packageRollbarHS) (Traced "hpack") "hpack"

buildDir :: FilePath
buildDir = "_build"

buildRollbarHS :: FilePath
buildRollbarHS = buildDir </> packageRollbarHS

distRollbarHS :: FilePath
distRollbarHS = packageRollbarHS </> "dist"

ghciFlags :: [String]
ghciFlags =
  [ "-ferror-spans"
  , "-fno-break-on-exception"
  , "-fno-break-on-error"
  , "-fno-code"
  , "-j"
  , "-v1"
  ]

packageDir :: FilePath
packageDir = "packages"

packageRollbarHS :: FilePath
packageRollbarHS = packageDir </> rollbarHS

rollbarHS :: FilePath
rollbarHS = "rollbar-hs"

rollbarHSVersion :: String
rollbarHSVersion = "0.3.1.0"
