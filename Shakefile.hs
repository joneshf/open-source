{-# LANGUAGE PackageImports #-}
module Main (main) where

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
import "typed-process" System.Process.Typed (runProcess_, shell)

import qualified "this" Shake.Cabal
import qualified "this" Shake.Circleci
import qualified "this" Shake.Dhall
import qualified "this" Shake.Haskell
import qualified "this" Shake.Package
import qualified "this" Shake.Yaml

main :: IO ()
main = do
  Shake.Package.writeDhall
  packages' <- getDirectoryFilesIO "" ["packages/*/shake.dhall"]
  packages <- traverse (detailed . input auto . pack . ("./" <>)) packages'
  let options = shakeOptions
        { shakeChange = ChangeModtimeAndDigest
        , shakeFiles = buildDir
        , shakeThreads = 0
        }
  shakeArgs options $ do
    phony "clean" (removeFilesAfter "" [buildDir])

    phony "shell" (runAfter $ runProcess_ $ shell "nix-shell --pure")

    Shake.Cabal.rules buildDir

    Shake.Circleci.rules packageDir packages

    Shake.Dhall.rules buildDir

    Shake.Haskell.rules buildDir packageDir packages

    Shake.Package.rules buildDir packageDir packages

    Shake.Yaml.rules buildDir

buildDir :: FilePath
buildDir = "_build"

packageDir :: FilePath
packageDir = "packages"
