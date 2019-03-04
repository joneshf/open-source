{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Main (main) where

import "shake" Development.Shake          ((%>))
import "shake" Development.Shake.FilePath ((<.>), (</>))

import qualified "base" Control.Monad
import qualified "base" Control.Monad.Fail
import qualified "mtl" Control.Monad.State.Strict
import qualified "mtl" Control.Monad.Trans
import qualified "base" Data.Foldable
import qualified "unordered-containers" Data.HashMap.Strict
import qualified "unordered-containers" Data.HashSet
import qualified "base" Data.Maybe
import qualified "text" Data.Text
import qualified "base" Data.Traversable
import qualified "shake" Development.Shake
import qualified "shake" Development.Shake.Classes
import qualified "shake" Development.Shake.FilePath
import qualified "dhall" Dhall
import qualified "dhall" Dhall.Core
import qualified "dhall" Dhall.Parser
import qualified "dhall" Dhall.TypeCheck
import qualified "base" GHC.Exts
import qualified "base" GHC.Generics

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
  let psURIs :: Data.HashMap.Strict.HashMap (Name, Version) URI
      psURIs = flip foldMap artifacts $ \case
        PureScriptProgram { dependencies } -> flip foldMap dependencies $ \case
          PureScript { name, uri, version } ->
            Data.HashMap.Strict.singleton (name, version) uri

  Development.Shake.shakeArgs options $ do

    binDir </> "purs/*/purs" %> \out -> do
      let untar = downloadDir </> "purs" </> version </> platform </> "purs"
          version =
            Development.Shake.FilePath.takeFileName
              . Development.Shake.FilePath.takeDirectory
              $ out
      Development.Shake.need [untar]
      Development.Shake.copyFileChanged untar out

    dependenciesDir </> "PureScript/*/*//*.purs" %> \out -> do
      let name =
            Development.Shake.FilePath.takeDirectory1
              . Development.Shake.FilePath.dropDirectory1
              $ withoutDeps
          stamp =
            dependenciesDir </> "PureScript" </> name </> version </> ".stamp"
          version =
            Development.Shake.FilePath.takeDirectory1
              . Development.Shake.FilePath.dropDirectory1
              . Development.Shake.FilePath.dropDirectory1
              $ withoutDeps
          withoutDeps = drop (length dependenciesDir + 1) out
      Development.Shake.need [stamp]

    dependenciesDir </> "PureScript/*/*/.stamp" %> \out -> do
      let name =
            Development.Shake.FilePath.takeFileName
              . Development.Shake.FilePath.takeDirectory
              . Development.Shake.FilePath.takeDirectory
              $ out
          tar = downloadDir </> "PureScript" </> name </> version <.> "tar.gz"
          untar = Development.Shake.FilePath.takeDirectory out
          version =
            Development.Shake.FilePath.takeFileName
              . Development.Shake.FilePath.takeDirectory
              $ out
      Development.Shake.need [tar]
      Development.Shake.cmd_
        "tar zxf"
        [tar]
        "--directory"
        [untar]
        "--strip-components 1"
      Development.Shake.writeFile' out ""

    downloadDir </> "PureScript/*/*.tar.gz" %> \out -> do
      let name =
            Data.Text.pack
              . Development.Shake.FilePath.takeFileName
              . Development.Shake.FilePath.takeDirectory
              $ out
          version =
            Data.Text.pack
              . Development.Shake.FilePath.dropExtension
              . Development.Shake.FilePath.dropExtension
              . Development.Shake.FilePath.takeFileName
              $ out
      URI uri <-
        maybe
          ( fail
            $ unwords
            [ "No dependency for"
            , show (name <> Data.Text.pack "@" <> version) <> "."
            , "Please add a dependency for"
            , show (name <> Data.Text.pack "@" <> version)
            , "to"
            , show buildFile
            ]
          )
          pure
          (Data.HashMap.Strict.lookup (Name name, Version version) psURIs)
      Development.Shake.cmd_
        "curl --location"
        "--output"
        [out]
        "--silent"
        [Data.Text.unpack uri]

    downloadDir </> "purs/*/*" %> \out -> do
      let (parent, platform') = Development.Shake.FilePath.splitFileName out
          uri =
            "https://github.com/purescript/purescript/releases/download/v"
              <> version
              <> "/"
              <> platform'
          version =
             Development.Shake.FilePath.takeFileName
              . Development.Shake.FilePath.takeDirectory
              $ parent
      Development.Shake.cmd_ "curl --location --output" [out] "--silent" [uri]

    downloadDir </> "purs/*/*/purs" %> \out -> do
      let tar = downloadDir </> "purs" </> version </> platform <.> "tar.gz"
          untar = Development.Shake.FilePath.takeDirectory out
          version =
            Development.Shake.FilePath.takeFileName
              . Development.Shake.FilePath.takeDirectory
              . Development.Shake.FilePath.takeDirectory
              $ out
      Development.Shake.need [tar]
      Development.Shake.cmd_
        "tar zxf"
        [tar]
        "--directory"
        [untar]
        "--strip-components 1"

    names <- Data.Traversable.for artifacts $ \case
      PureScriptProgram
        { compiler
        , dependencies = dependencies'
        , main' = main'@FileModuleName { module' = module'' }
        , name = Name name'
        , src = src'
        } -> do

          let dependencies ::
                Data.HashMap.Strict.HashMap ModuleName (Dir, FileModuleName)
              dependencies = flip foldMap dependencies' $ \case
                PureScript
                  { modules
                  , name = Name name
                  , src = Dir src
                  , version = Version version
                  } ->
                    flip foldMap modules $ \x@FileModuleName { module' } ->
                      Data.HashMap.Strict.singleton
                        module'
                        ( Dir
                          $ Data.Text.pack
                          $ dependenciesDir
                          </> "PureScript"
                          </> Data.Text.unpack name
                          </> Data.Text.unpack version
                          </> Data.Text.unpack src
                        , x
                        )

          Data.Text.unpack name' %> \out -> do
            Development.Shake.need [buildFile]
            modules' <-
              Control.Monad.State.Strict.execStateT
                (transitiveImports buildFile dependencies src' main')
                mempty
            let modules =
                  Data.HashSet.fromList (Data.HashMap.Strict.elems modules')
            compile binDir buildDir compiler modules module'' out

          pure (Name name')

    Development.Shake.want (fmap (\(Name x) -> Data.Text.unpack x) names)

    Development.Shake.phony
      "clean"
      (Development.Shake.removeFilesAfter buildDir ["//*"])

compile ::
  FilePath ->
  FilePath ->
  Version ->
  Data.HashSet.HashSet File ->
  ModuleName ->
  FilePath ->
  Development.Shake.Action ()
compile bin buildDir (Version compiler) modules (ModuleName module') out = do
  Development.Shake.need (purs : Data.HashSet.toList psFiles)
  Development.Shake.cmd_
    (Development.Shake.Traced "purs compile")
    [purs]
    "compile"
    "--output"
    [output]
    (Data.HashSet.toList psFiles)
  Development.Shake.writeFileLines
    out
    [ "#!/usr/bin/env node"
    , show "use strict" <> ";"
    , "require("
      <> show ("." </> output </> Data.Text.unpack module')
      <> ").main()"
    ]
  Development.Shake.cmd_ "chmod 0755" [out]
  where
  psFiles = Data.HashSet.map (\(File f) -> Data.Text.unpack f) modules
  purs = bin </> "purs" </> Data.Text.unpack compiler </> "purs"
  output = buildDir </> "PureScript/output" </> Data.Text.unpack compiler

transitiveImports ::
  FilePath ->
  Data.HashMap.Strict.HashMap ModuleName (Dir, FileModuleName) ->
  Dir ->
  FileModuleName ->
  Control.Monad.State.Strict.StateT
    (Data.HashMap.Strict.HashMap ModuleName File)
    Development.Shake.Action
    ()
transitiveImports buildFile dependencies (Dir src') FileModuleName { file = File file', module' = module'' } = do
  found <- Control.Monad.State.Strict.gets (Data.HashMap.Strict.member module'')
  Control.Monad.unless found $ do
    psLines <-
      Control.Monad.Trans.lift (Development.Shake.readFileLines fileName')
    Data.Foldable.for_ (imports psLines) $ \x' ->
      case Data.HashMap.Strict.lookup x' dependencies of
        Just (src, x) -> transitiveImports buildFile dependencies src x
        Nothing       -> Control.Monad.unless (prim x') (go x')
    Control.Monad.State.Strict.modify'
      (Data.HashMap.Strict.insert module'' (File $ Data.Text.pack fileName'))
  where
  fileName' :: String
  fileName' = Data.Text.unpack src' </> Data.Text.unpack file'

  moduleNotFound :: (Control.Monad.Fail.MonadFail f) => ModuleName -> f a
  moduleNotFound (ModuleName name) =
    fail
      ( unwords
        [ "Could not find module"
        , show name <> "."
        , "Please add the dependency to"
        , show buildFile
        ]
      )

  go ::
    ModuleName ->
    Control.Monad.State.Strict.StateT
      (Data.HashMap.Strict.HashMap ModuleName File)
      Development.Shake.Action
      ()
  go module' = do
    let file@(File rawFile) = fileFromModuleName module'
        fileModuleName' = FileModuleName { file, module' }
    fileExists <-
      Control.Monad.Trans.lift
        ( Development.Shake.doesFileExist
          $ Data.Text.unpack src'
          </> Data.Text.unpack rawFile
        )
    if fileExists
      then transitiveImports buildFile dependencies (Dir src') fileModuleName'
      else moduleNotFound module'

fileFromModuleName :: ModuleName -> File
fileFromModuleName (ModuleName name) =
  File
    $ Data.Text.pack
    $ Development.Shake.FilePath.joinPath
      (Data.Text.unpack <$> Data.Text.splitOn (Data.Text.pack ".") name)
    <.> "purs"

imports :: [String] -> Data.HashSet.HashSet ModuleName
imports =
  Data.HashSet.fromList . Data.Maybe.mapMaybe (parsePrefix "import")

parsePrefix :: String -> String -> Maybe ModuleName
parsePrefix needle' haystack = case words haystack of
  (needle:name:_) | needle == needle' -> pure (ModuleName $ Data.Text.pack name)
  _                                   -> Nothing

prim :: ModuleName -> Bool
prim = (`Data.HashSet.member` primModules)

primModules :: Data.HashSet.HashSet ModuleName
primModules =
  Data.HashSet.fromList
    [ ModuleName (Data.Text.pack "Prim")
    , ModuleName (Data.Text.pack "Prim.Boolean")
    , ModuleName (Data.Text.pack "Prim.Ordering")
    , ModuleName (Data.Text.pack "Prim.Row")
    , ModuleName (Data.Text.pack "Prim.RowList")
    , ModuleName (Data.Text.pack "Prim.Symbol")
    , ModuleName (Data.Text.pack "Prim.TypeError")
    ]

newtype Dir = Dir Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype File = File Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype ModuleName = ModuleName Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Name = Name Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

data FileModuleName
  = FileModuleName
    { file    :: File
    , module' :: ModuleName
    }
  deriving (GHC.Generics.Generic, Eq)

fileModuleName :: Dhall.Type FileModuleName
fileModuleName = Dhall.record $ do
  file <- Dhall.field (Data.Text.pack "file") (fmap File Dhall.strictText)
  module' <-
    Dhall.field (Data.Text.pack "module") (fmap ModuleName Dhall.strictText)
  pure FileModuleName { file, module' }

newtype URI = URI Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Version = Version Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Config = Config [Artifact]

config :: Dhall.Type Config
config = fmap Config (Dhall.list artifact)

data Artifact
  = PureScriptProgram
    { compiler     :: Version
    , dependencies :: [Dependency]
    , main'        :: FileModuleName
    , name         :: Name
    , src          :: Dir
    }

artifact :: Dhall.Type Artifact
artifact = union [(Data.Text.pack "PureScript/program", pureScriptProgram)]
  where
  pureScriptProgram :: Dhall.Type Artifact
  pureScriptProgram = Dhall.record $ do
    compiler <-
      Dhall.field (Data.Text.pack "compiler") (fmap Version Dhall.strictText)
    dependencies <-
      Dhall.field (Data.Text.pack "dependencies") (Dhall.list dependency)
    main' <- Dhall.field (Data.Text.pack "main") fileModuleName
    name <- Dhall.field (Data.Text.pack "name") (fmap Name Dhall.strictText)
    src <- Dhall.field (Data.Text.pack "src") (fmap Dir Dhall.strictText)
    pure PureScriptProgram { compiler, dependencies, main', name, src }

data Dependency
  = PureScript
    { modules :: [FileModuleName]
    , name    :: Name
    , src     :: Dir
    , uri     :: URI
    , version :: Version
    }

dependency :: Dhall.Type Dependency
dependency = union [(Data.Text.pack "PureScript", pureScript)]
  where
  pureScript :: Dhall.Type Dependency
  pureScript = Dhall.record $ do
    modules <- Dhall.field (Data.Text.pack "modules") (Dhall.list fileModuleName)
    name <- Dhall.field (Data.Text.pack "name") (fmap Name Dhall.strictText)
    src <- Dhall.field (Data.Text.pack "src") (fmap Dir Dhall.strictText)
    uri <- Dhall.field (Data.Text.pack "uri") (fmap URI Dhall.strictText)
    version <-
      Dhall.field (Data.Text.pack "version") (fmap Version Dhall.strictText)
    pure PureScript { modules, name, src, uri, version }

union :: forall a. [(Data.Text.Text, Dhall.Type a)] -> Dhall.Type a
union xs = Dhall.Type { Dhall.expected, Dhall.extract }
  where
  expected :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  expected =
    Dhall.Core.Union (GHC.Exts.fromList $ (fmap . fmap) Dhall.expected xs)
  extract :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X -> Maybe a
  extract = \case
    Dhall.Core.UnionLit alternate x _ -> do
      ty <- lookup alternate xs
      Dhall.extract ty x
    _ -> Nothing
