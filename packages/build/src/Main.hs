{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Main (main) where

import "shake" Development.Shake.FilePath ((</>))

import qualified "this" Build
import qualified "this" Build.PureScript
import qualified "base" Data.Maybe
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
  let psPrograms :: [Build.PureScript.Program]
      psPrograms = flip Data.Maybe.mapMaybe artifacts $ \case
        PureScriptProgram program -> Just program

  Development.Shake.shakeArgs options $ do

    names <-
      Build.PureScript.rules
        psPrograms
        binDir
        buildDir
        buildFile
        dependenciesDir
        downloadDir
        platform

    Development.Shake.want (fmap (\(Build.Name x) -> Data.Text.unpack x) names)

    Development.Shake.phony
      "clean"
      (Development.Shake.removeFilesAfter buildDir ["//*"])

newtype Config = Config [Artifact]

config :: Dhall.Type Config
config =
  fmap Config (Dhall.list artifact)

newtype Artifact = PureScriptProgram Build.PureScript.Program

artifact :: Dhall.Type Artifact
artifact =
  Dhall.union
    ( Dhall.constructor
      (Data.Text.pack "PureScript/program")
      (fmap PureScriptProgram Build.PureScript.program)
    )
