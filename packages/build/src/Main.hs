{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Main (main) where

import "shake" Development.Shake.FilePath ((</>))

import qualified "this" Build
import qualified "this" Build.PureScript
import qualified "unordered-containers" Data.HashMap.Strict
import qualified "text" Data.Text
import qualified "shake" Development.Shake
import qualified "dhall" Dhall

main :: IO ()
main = do
  let binDir = buildDir </> "bin"
      buildDir = ".build"
      buildFile = "build.dhall"
      dependenciesDir = buildDir </> "dependencies"
      downloadDir = buildDir </> "download"
      options =
        Development.Shake.shakeOptions
          { Development.Shake.shakeChange =
            Development.Shake.ChangeModtimeAndDigest
          , Development.Shake.shakeFiles = buildDir
          , Development.Shake.shakeReport =
            [ buildDir </> "report.html"
            , buildDir </> "report.js"
            , buildDir </> "report.trace"
            ]
          , Development.Shake.shakeThreads = 0
          }
      platform = "linux64"
  Config artifacts <- Dhall.detailed (Dhall.inputFile config buildFile)
  let psURIs :: Data.HashMap.Strict.HashMap (Build.Name, Build.Version) Build.URI
      psURIs = Build.psURIs artifacts

  Development.Shake.shakeArgs options $ do

    names <-
      Build.PureScript.rules
        artifacts
        binDir
        buildDir
        buildFile
        dependenciesDir
        downloadDir
        platform
        psURIs

    Development.Shake.want (fmap (\(Build.Name x) -> Data.Text.unpack x) names)

    Development.Shake.phony
      "clean"
      (Development.Shake.removeFilesAfter buildDir ["//*"])

newtype Config = Config [Build.Artifact]

config :: Dhall.Type Config
config = fmap Config (Dhall.list Build.artifact)
