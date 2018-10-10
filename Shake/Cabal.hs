{-# LANGUAGE PackageImports #-}
module Shake.Cabal (rules) where

import "shake" Development.Shake
    ( CmdOption(Cwd, EchoStdout, FileStdout, Traced)
    , Rules
    , cmd_
    , copyFileChanged
    , need
    , (%>)
    , (<//>)
    )
import "shake" Development.Shake.FilePath
    ( dropDirectory1
    , dropExtension
    , takeDirectory
    , (-<.>)
    , (</>)
    )

rules :: FilePath -> Rules ()
rules buildDir = do
  buildDir </> ".update" %> \out -> do
    need ["Shake/Cabal.hs"]
    cmd_ (FileStdout out) (Traced "cabal update") "cabal update"

  buildDir <//> "*.cabal.format" %> \out -> do
    need ["Shake/Cabal.hs"]
    -- Skip over formatting cabal files.
    -- Although `cabal format` does what it says it will,
    -- it's riddled with bugs.
    -- Most annoyingly, it drops comments.
    -- Once that's fixed, use it.
    copyFileChanged ((dropDirectory1 . dropExtension) out) out

  buildDir <//> "*.cabal.lint" %> \out -> do
    let input = (dropDirectory1 . dropExtension) out
    need ["Shake/Cabal.hs", input, out -<.> "format"]
    cmd_
      (Cwd $ takeDirectory input)
      (EchoStdout True)
      (FileStdout out)
      (Traced "cabal check")
      "cabal check"
