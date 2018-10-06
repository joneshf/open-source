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
import "typed-process" System.Process.Typed
    ( proc
    , runProcess_
    , setWorkingDir
    , shell
    )

main :: IO ()
main = do
  shakeVersion <- getHashedShakeVersion ["Shakefile.hs"]
  let options = shakeOptions { shakeFiles, shakeThreads, shakeVersion }
      shakeFiles = buildDir
      shakeThreads = 0
  shakeArgs options $ do
    want ["build"]

    phony "build"
      ( need
        [ buildKatipRollbar </> ".build"
        , buildRollbarHS </> ".build"
        , buildWaiMiddlewareRollbar </> ".build"
        ]
      )

    phony "clean"
      ( removeFilesAfter
        ""
        [ buildDir
        , distKatipRollbar
        , distRollbarHS
        , distWaiMiddlewareRollbar
        ]
      )

    phony "sdist"
      ( need
        [ distKatipRollbar
          </> katipRollbar
          <> "-"
          <> katipRollbarVersion
          <> ".tar.gz"
        , distRollbarHS </> rollbarHS <> "-" <> rollbarHSVersion <> ".tar.gz"
        , distWaiMiddlewareRollbar
          </> waiMiddlewareRollbar
          <> "-"
          <> waiMiddlewareRollbarVersion
          <> ".tar.gz"
        ]
      )

    phony "shell" (runAfter $ runProcess_ $ shell "nix-shell --pure")

    phony "test" (need [distRollbarHS </> "build/doc-test/doc-test.out"])

    phony "upload-to-hackage"
      ( need
        [ distKatipRollbar </> katipRollbar <> "-" <> katipRollbarVersion
        , distRollbarHS </> rollbarHS <> "-" <> rollbarHSVersion
        , distWaiMiddlewareRollbar
          </> waiMiddlewareRollbar
          <> "-"
          <> waiMiddlewareRollbarVersion
        ]
      )

    phony ("watch-" <> katipRollbar) $ do
      need [buildKatipRollbar </> ".configure"]
      runAfter
        ( runProcess_
        $ setWorkingDir packageKatipRollbar
        $ proc
          "ghcid"
          [ "--command"
          , "cabal repl lib:"
            <> katipRollbar
            <> " --builddir "
            <> distKatipRollbar
            <> " --ghc-options '"
            <> unwords ghciFlags
            <> "'"
          ]
        )

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
            <> " --builddir "
            <> distRollbarHS
            <> " --ghc-options '"
            <> unwords ghciFlags
            <> "'"
          ]
        )

    phony ("watch-" <> waiMiddlewareRollbar) $ do
      need [buildWaiMiddlewareRollbar </> ".configure"]
      runAfter
        ( runProcess_
        $ setWorkingDir packageWaiMiddlewareRollbar
        $ proc
          "ghcid"
          [ "--command"
          , "cabal repl lib:"
            <> waiMiddlewareRollbar
            <> " --builddir "
            <> distWaiMiddlewareRollbar
            <> " --ghc-options '"
            <> unwords ghciFlags
            <> "'"
          ]
        )

    buildDir </> ".update" %> \out ->
      cmd (FileStdout out) (Traced "cabal update") "cabal update"

    buildKatipRollbar </> ".build" %> \out -> do
      srcs <- getDirectoryFiles "" [packageKatipRollbar </> "src//*.hs"]
      need ((buildKatipRollbar </> ".configure") : srcs)
      cmd
        (Cwd packageKatipRollbar)
        (FileStdout out)
        (Traced "cabal build")
        "cabal build"
        "--builddir"
        [distKatipRollbar]

    buildRollbarHS </> ".build" %> \out -> do
      srcs <- getDirectoryFiles "" [packageRollbarHS </> "src//*.hs"]
      need ((buildRollbarHS </> ".configure") : srcs)
      cmd
        (Cwd packageRollbarHS)
        (FileStdout out)
        (Traced "cabal build")
        "cabal build"
        "--builddir"
        [distRollbarHS]

    buildWaiMiddlewareRollbar </> ".build" %> \out -> do
      srcs <- getDirectoryFiles "" [packageWaiMiddlewareRollbar </> "src//*.hs"]
      need ((buildWaiMiddlewareRollbar </> ".configure") : srcs)
      cmd
        (Cwd packageWaiMiddlewareRollbar)
        (FileStdout out)
        (Traced "cabal build")
        "cabal build"
        "--builddir"
        [distWaiMiddlewareRollbar]

    buildKatipRollbar </> ".check" %> \out -> do
      need [packageKatipRollbar </> katipRollbar <.> "cabal"]
      cmd
        (Cwd packageKatipRollbar)
        (EchoStdout True)
        (FileStdout out)
        (Traced "cabal check")
        "cabal check"

    buildRollbarHS </> ".check" %> \out -> do
      need [packageRollbarHS </> rollbarHS <.> "cabal"]
      cmd
        (Cwd packageRollbarHS)
        (EchoStdout True)
        (FileStdout out)
        (Traced "cabal check")
        "cabal check"

    buildWaiMiddlewareRollbar </> ".check" %> \out -> do
      need [packageWaiMiddlewareRollbar </> waiMiddlewareRollbar <.> "cabal"]
      cmd
        (Cwd packageWaiMiddlewareRollbar)
        (EchoStdout True)
        (FileStdout out)
        (Traced "cabal check")
        "cabal check"

    buildKatipRollbar </> ".configure" %> \out -> do
      need
        [ buildDir </> ".update"
        , packageKatipRollbar </> katipRollbar <.> "cabal"
        ]
      cmd
        (Cwd packageKatipRollbar)
        (FileStdout out)
        (Traced "cabal configure")
        "cabal configure"
        "--builddir"
        [distKatipRollbar]

    buildRollbarHS </> ".configure" %> \out -> do
      need [buildDir </> ".update", packageRollbarHS </> rollbarHS <.> "cabal"]
      cmd
        (Cwd packageRollbarHS)
        (FileStdout out)
        (Traced "cabal configure")
        "cabal configure"
        "--builddir"
        [distRollbarHS]
        "--enable-tests"

    buildWaiMiddlewareRollbar </> ".configure" %> \out -> do
      need
        [ buildDir </> ".update"
        , packageWaiMiddlewareRollbar </> waiMiddlewareRollbar <.> "cabal"
        ]
      cmd
        (Cwd packageWaiMiddlewareRollbar)
        (FileStdout out)
        (Traced "cabal configure")
        "cabal configure"
        "--builddir"
        [distWaiMiddlewareRollbar]

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
        "--builddir"
        [distRollbarHS]

    distRollbarHS </> "build/doc-test/doc-test.out" %> \out -> do
      need [dropExtension out]
      cmd_
        (Cwd packageRollbarHS)
        (FileStdout out)
        (Traced "rollbar doc-test")
        [(dropDirectory1 . dropDirectory1 . dropExtension) out]

    distKatipRollbar </> katipRollbar <> "-" <> katipRollbarVersion <.> "tar.gz" %> \_ -> do
      srcs <- getDirectoryFiles "" [packageKatipRollbar </> "src//*.hs"]
      need
        ( (buildKatipRollbar </> ".check")
        : (buildKatipRollbar </> ".configure")
        : srcs
        )
      cmd_
        (Cwd packageKatipRollbar)
        (Traced "cabal sdist")
        "cabal sdist"
        "--builddir"
        [distKatipRollbar]

    distRollbarHS </> rollbarHS <> "-" <> rollbarHSVersion <.> "tar.gz" %> \_ -> do
      srcs <- getDirectoryFiles "" [packageRollbarHS </> "src//*.hs"]
      need
        ( (buildRollbarHS </> ".check")
        : (buildRollbarHS </> ".configure")
        : srcs
        )
      cmd_
        (Cwd packageRollbarHS)
        (Traced "cabal sdist")
        "cabal sdist"
        "--builddir"
        [distRollbarHS]

    distWaiMiddlewareRollbar </> waiMiddlewareRollbar <> "-" <> waiMiddlewareRollbarVersion <.> "tar.gz" %> \_ -> do
      srcs <- getDirectoryFiles "" [packageWaiMiddlewareRollbar </> "src//*.hs"]
      need
        ( (buildWaiMiddlewareRollbar </> ".check")
        : (buildWaiMiddlewareRollbar </> ".configure")
        : srcs
        )
      cmd_
        (Cwd packageWaiMiddlewareRollbar)
        (Traced "cabal sdist")
        "cabal sdist"
        "--builddir"
        [distWaiMiddlewareRollbar]

    distKatipRollbar </> katipRollbar <> "-" <> katipRollbarVersion %> \out -> do
      (Exit x, Stdout result) <-
        cmd (Traced "cabal info") "cabal info" [takeFileName out]
      case x of
        ExitFailure _ -> do
          need [buildKatipRollbar </> ".build", out <.> "tar.gz"]
          cmd_ (Traced "cabal upload") "cabal upload" [out <.> "tar.gz"]
          writeFile' out ""
        ExitSuccess -> writeFile' out result

    distRollbarHS </> rollbarHS <> "-" <> rollbarHSVersion %> \out -> do
      (Exit x, Stdout result) <-
        cmd (Traced "cabal info") "cabal info" [takeFileName out]
      case x of
        ExitFailure _ -> do
          need [buildRollbarHS </> ".build", out <.> "tar.gz"]
          cmd_ (Traced "cabal upload") "cabal upload" [out <.> "tar.gz"]
          writeFile' out ""
        ExitSuccess -> writeFile' out result

    distWaiMiddlewareRollbar </> waiMiddlewareRollbar <> "-" <> waiMiddlewareRollbarVersion %> \out -> do
      (Exit x, Stdout result) <-
        cmd (Traced "cabal info") "cabal info" [takeFileName out]
      case x of
        ExitFailure _ -> do
          need [buildWaiMiddlewareRollbar </> ".build", out <.> "tar.gz"]
          cmd_ (Traced "cabal upload") "cabal upload" [out <.> "tar.gz"]
          writeFile' out ""
        ExitSuccess -> writeFile' out result

    packageKatipRollbar </> katipRollbar <.> "cabal" %> \out -> do
      need [replaceFileName out "package.yaml"]
      cmd_ (Cwd packageKatipRollbar) (Traced "hpack") "hpack"

    packageRollbarHS </> rollbarHS <.> "cabal" %> \out -> do
      need [replaceFileName out "package.yaml"]
      cmd_ (Cwd packageRollbarHS) (Traced "hpack") "hpack"

buildDir :: FilePath
buildDir = "_build"

buildKatipRollbar :: FilePath
buildKatipRollbar = buildDir </> packageKatipRollbar

buildRollbarHS :: FilePath
buildRollbarHS = buildDir </> packageRollbarHS

buildWaiMiddlewareRollbar :: FilePath
buildWaiMiddlewareRollbar = buildDir </> packageWaiMiddlewareRollbar

distKatipRollbar :: FilePath
distKatipRollbar = packageKatipRollbar </> "dist"

distRollbarHS :: FilePath
distRollbarHS = packageRollbarHS </> "dist"

distWaiMiddlewareRollbar :: FilePath
distWaiMiddlewareRollbar = packageWaiMiddlewareRollbar </> "dist"

ghciFlags :: [String]
ghciFlags =
  [ "-ferror-spans"
  , "-fno-break-on-exception"
  , "-fno-break-on-error"
  , "-fno-code"
  , "-j"
  , "-v1"
  ]

katipRollbar :: FilePath
katipRollbar = "katip-rollbar"

katipRollbarVersion :: FilePath
katipRollbarVersion = "0.3.0.1"

packageDir :: FilePath
packageDir = "packages"

packageKatipRollbar :: FilePath
packageKatipRollbar = packageDir </> katipRollbar

packageRollbarHS :: FilePath
packageRollbarHS = packageDir </> rollbarHS

packageWaiMiddlewareRollbar :: FilePath
packageWaiMiddlewareRollbar = packageDir </> waiMiddlewareRollbar

rollbarHS :: FilePath
rollbarHS = "rollbar-hs"

rollbarHSVersion :: String
rollbarHSVersion = "0.3.1.0"

waiMiddlewareRollbar :: FilePath
waiMiddlewareRollbar = "wai-middleware-rollbar"

waiMiddlewareRollbarVersion :: String
waiMiddlewareRollbarVersion = "0.11.0"
