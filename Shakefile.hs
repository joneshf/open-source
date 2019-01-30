{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Main (main) where

import "mtl" Control.Monad.Reader           (lift, runReaderT)
import "text" Data.Text                     (pack)
import "shake" Development.Shake
    ( Change(ChangeModtimeAndDigest)
    , ShakeOptions(shakeChange, shakeFiles, shakeThreads)
    , getDirectoryFilesIO
    , phony
    , removeFilesAfter
    , runAfter
    , shakeArgs
    , shakeOptions
    )
import "dhall" Dhall                        (auto, detailed, input)
import "this" Shake.Package                 (Package)
import "typed-process" System.Process.Typed (runProcess_, shell)

import qualified "this" Shake.Cabal
import qualified "this" Shake.Circleci
import qualified "this" Shake.Dhall
import qualified "this" Shake.Haskell
import qualified "this" Shake.Package
import qualified "this" Shake.Yaml

data Env
  = Env
    { binDir   :: FilePath
    , buildDir   :: FilePath
    , packageDir :: FilePath
    , packages   :: [Package]
    }

main :: IO ()
main = do
  Shake.Package.writeDhall
  packages' <- getDirectoryFilesIO "" ["packages/*/shake.dhall"]
  packages <- traverse (detailed . input auto . pack . ("./" <>)) packages'
  let binDir = "bin"
      buildDir = "_build"
      env = Env { binDir, buildDir, packageDir, packages}
      options = shakeOptions
        { shakeChange = ChangeModtimeAndDigest
        , shakeFiles = buildDir
        , shakeThreads = 0
        }
      packageDir = "packages"
  shakeArgs options $ flip runReaderT env $ do
    lift $ phony "clean" (removeFilesAfter "" [binDir, buildDir])

    lift $ phony "shell" (runAfter $ runProcess_ $ shell "nix-shell --pure")

    Shake.Cabal.rules

    Shake.Circleci.rules

    Shake.Dhall.rules

    Shake.Haskell.rules

    Shake.Package.rules

    Shake.Yaml.rules
