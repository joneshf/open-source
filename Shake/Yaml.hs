{-# LANGUAGE PackageImports #-}
module Shake.Yaml (rules) where

import "shake" Development.Shake
    ( CmdOption(Traced)
    , Rules
    , cmd_
    , copyFileChanged
    , need
    , needed
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath
    ( dropDirectory1
    , dropExtension
    , (-<.>)
    )

rules :: FilePath -> Rules ()
rules buildDir = do
  buildDir <//> "*.yaml.format" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".prettierrc", "Shake/Yaml.hs"]
    cmd_ (Traced "prettier") "prettier" "--write" [input]
    copyFileChanged input out
    needed [input]

  buildDir <//> "*.yaml.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need [".yamllint", "Shake/Yaml.hs", input, out -<.> "format"]
    cmd_ (Traced "yamllint") "yamllint" "--strict" [input]
    copyFileChanged input out
