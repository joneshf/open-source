{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Main (main) where

import "mtl" Control.Monad.Reader
    ( lift
    , runReaderT
    , withReaderT
    )
import "base" Data.Maybe                            (mapMaybe)
import "text" Data.Text                             (pack)
import "base" Data.Traversable                      (for)
import "shake" Development.Shake
    ( Change(ChangeModtimeAndDigest)
    , ShakeOptions(shakeChange, shakeFiles, shakeReport, shakeShare, shakeThreads)
    , getDirectoryFilesIO
    , phony
    , removeFilesAfter
    , runAfter
    , shakeArgs
    , shakeOptions
    )
import "shake" Development.Shake.FilePath           ((</>))
import "dhall" Dhall                                (auto, detailed, input)
import "this" Shake.Package                         (Package)
import "xdg-basedir" System.Environment.XDG.BaseDir (getUserCacheDir)
import "typed-process" System.Process.Typed         (runProcess_, shell)

import qualified "this" Shake.Cabal
import qualified "this" Shake.Dhall
import qualified "this" Shake.Haskell
import qualified "this" Shake.JavaScript
import qualified "this" Shake.Package
import qualified "this" Shake.Package.Haskell
import qualified "this" Shake.Yaml

data Env a
  = Env
    { binDir     :: FilePath
    , buildDir   :: FilePath
    , packageDir :: FilePath
    , packages   :: a
    }
  deriving (Functor)

main :: IO ()
main = do
  Shake.Package.writeDhall
  packages' <- getDirectoryFilesIO "" ["packages/*/shake.dhall"]
  packages <- for packages' $ \package ->
    detailed (input Shake.Package.packageType $ pack $ "." </> package)
  shareDir <- getUserCacheDir ("shake" </> "open-source")
  let binDir = "bin"
      buildDir = "_build"
      env = Env { binDir, buildDir, packageDir, packages}
      options = shakeOptions
        { shakeChange = ChangeModtimeAndDigest
        , shakeFiles = buildDir
        , shakeReport =
          [ buildDir </> "report.html"
          , buildDir </> "report.js"
          , buildDir </> "report.json"
          , buildDir </> "report.trace"
          ]
        , shakeShare = pure shareDir
        , shakeThreads = 0
        }
      packageDir = "packages"
  shakeArgs options $ flip runReaderT env $ do
    lift $ phony "clean" (removeFilesAfter "" [binDir, buildDir])

    lift $ phony "shell" (runAfter $ runProcess_ $ shell "nix-shell --pure")

    Shake.Cabal.rules

    Shake.Dhall.rules

    withReaderT ((fmap . mapMaybe) Shake.Package.haskell) Shake.Haskell.rules

    withReaderT ((fmap . mapMaybe) Shake.Package.javaScript) Shake.JavaScript.rules

    Shake.Package.rules

    Shake.Yaml.rules
