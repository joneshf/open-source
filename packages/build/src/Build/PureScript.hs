{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Build.PureScript (Program, program, rules) where

import "shake" Development.Shake          ((%>))
import "shake" Development.Shake.FilePath ((<.>), (</>))

import qualified "this" Build
import qualified "base" Control.Monad
import qualified "base" Control.Monad.Fail
import qualified "mtl" Control.Monad.State.Strict
import qualified "mtl" Control.Monad.Trans
import qualified "base" Data.Foldable
import qualified "base" Data.Functor
import qualified "unordered-containers" Data.HashMap.Strict
import qualified "unordered-containers" Data.HashSet
import qualified "base" Data.Maybe
import qualified "text" Data.Text
import qualified "base" Data.Traversable
import qualified "shake" Development.Shake
import qualified "shake" Development.Shake.Classes
import qualified "shake" Development.Shake.FilePath
import qualified "dhall" Dhall
import qualified "base" GHC.Generics

data Program
  = Program
    { compiler     :: Build.Version
    , dependencies :: [Dependency]
    , main'        :: FileModuleName
    , name         :: Build.Name
    , src          :: Build.Dir
    }

program :: Dhall.Type Program
program = Dhall.record $ do
  compiler <-
    Dhall.field (Data.Text.pack "compiler") (fmap Build.Version Dhall.strictText)
  dependencies <-
    Dhall.field (Data.Text.pack "dependencies") (Dhall.list dependency)
  main' <- Dhall.field (Data.Text.pack "main") fileModuleName
  name <- Dhall.field (Data.Text.pack "name") (fmap Build.Name Dhall.strictText)
  src <- Dhall.field (Data.Text.pack "src") (fmap Build.Dir Dhall.strictText)
  pure Program { compiler, dependencies, main', name, src }

data Dependency
  = PureScript
    { modules :: [FileModuleName]
    , name    :: Build.Name
    , src     :: Build.Dir
    , uri     :: Build.URI
    , version :: Build.Version
    }

dependency :: Dhall.Type Dependency
dependency =
  Dhall.union (Dhall.constructor (Data.Text.pack "PureScript") pureScript)
  where
  pureScript :: Dhall.Type Dependency
  pureScript = Dhall.record $ do
    modules <- Dhall.field (Data.Text.pack "modules") (Dhall.list fileModuleName)
    name <- Dhall.field (Data.Text.pack "name") (fmap Build.Name Dhall.strictText)
    src <- Dhall.field (Data.Text.pack "src") (fmap Build.Dir Dhall.strictText)
    uri <- Dhall.field (Data.Text.pack "uri") (fmap Build.URI Dhall.strictText)
    version <-
      Dhall.field (Data.Text.pack "version") (fmap Build.Version Dhall.strictText)
    pure PureScript { modules, name, src, uri, version }

data FileModuleName
  = FileModuleName
    { file    :: Build.File
    , module' :: ModuleName
    }
  deriving (GHC.Generics.Generic, Eq)

fileModuleName :: Dhall.Type FileModuleName
fileModuleName = Dhall.record $ do
  file <- Dhall.field (Data.Text.pack "file") (fmap Build.File Dhall.strictText)
  module' <-
    Dhall.field (Data.Text.pack "module") (fmap ModuleName Dhall.strictText)
  pure FileModuleName { file, module' }

newtype ModuleName = ModuleName Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

rules ::
  Traversable f =>
  f Program ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  Development.Shake.Rules (f Build.Name)
rules artifacts binDir buildDir buildFile dependenciesDir downloadDir platform = do
  let uris :: Data.HashMap.Strict.HashMap (Build.Name, Build.Version) Build.URI
      uris = flip foldMap artifacts $ \case
        Program { dependencies } -> flip foldMap dependencies $ \case
          PureScript { name, uri, version } ->
            Data.HashMap.Strict.singleton (name, version) uri

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
    Build.URI uri <-
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
        (Data.HashMap.Strict.lookup (Build.Name name, Build.Version version) uris)
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

  Data.Traversable.for artifacts $ \case
    Program
      { compiler
      , dependencies = dependencies'
      , main' = main'@FileModuleName { module' = module'' }
      , name = Build.Name name'
      , src = src'
      } -> do

        let dependencies ::
              Data.HashMap.Strict.HashMap ModuleName (Build.Dir, FileModuleName)
            dependencies = flip foldMap dependencies' $ \case
              PureScript
                { modules
                , name = Build.Name name
                , src = Build.Dir src
                , version = Build.Version version
                } ->
                  flip foldMap modules $ \x@FileModuleName { module' } ->
                    Data.HashMap.Strict.singleton
                      module'
                      ( Build.Dir
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
          modules <-
            Control.Monad.State.Strict.execStateT
              (transitiveImports buildFile dependencies src' main')
              mempty
          compile binDir buildDir compiler modules module'' out

        pure (Build.Name name')

compile ::
  FilePath ->
  FilePath ->
  Build.Version ->
  Data.HashMap.Strict.HashMap ModuleName Build.File ->
  ModuleName ->
  FilePath ->
  Development.Shake.Action ()
compile bin buildDir (Build.Version compiler) modules (ModuleName module') out = do
  Development.Shake.need (purs : Data.HashSet.toList psFiles)
  Development.Shake.cmd_
    (Development.Shake.Traced "purs compile")
    [purs]
    "compile"
    "--output"
    [output]
    (Data.HashSet.toList psFiles)
  Development.Shake.cmd_
    (Development.Shake.Traced "purs bundle")
    [purs]
    "bundle"
    "--main"
    [Data.Text.unpack module']
    "--module"
    [Data.Text.unpack module']
    "--output"
    [out]
    (Data.HashSet.toList outputForeigns)
    (Data.HashSet.toList outputIndexes)
  where
  psFiles =
    Data.HashSet.fromList
      ( Data.HashMap.Strict.elems
        $ fmap (\(Build.File f) -> Data.Text.unpack f) modules
      )
  purs = bin </> "purs" </> Data.Text.unpack compiler </> "purs"
  output = buildDir </> "PureScript/output" </> Data.Text.unpack compiler
  outputForeigns =
    Data.HashSet.map
      (\(ModuleName name) -> output </> Data.Text.unpack name </> "foreign.js")
      (Data.HashSet.fromMap (Data.Functor.void modules))
  outputIndexes =
    Data.HashSet.map
      (\(ModuleName name) -> output </> Data.Text.unpack name </> "index.js")
      (Data.HashSet.fromMap (Data.Functor.void modules))

transitiveImports ::
  FilePath ->
  Data.HashMap.Strict.HashMap ModuleName (Build.Dir, FileModuleName) ->
  Build.Dir ->
  FileModuleName ->
  Control.Monad.State.Strict.StateT
    (Data.HashMap.Strict.HashMap ModuleName Build.File)
    Development.Shake.Action
    ()
transitiveImports buildFile dependencies (Build.Dir src') FileModuleName { file = Build.File file', module' = module'' } = do
  found <- Control.Monad.State.Strict.gets (Data.HashMap.Strict.member module'')
  Control.Monad.unless found $ do
    psLines <-
      Control.Monad.Trans.lift (Development.Shake.readFileLines fileName')
    Data.Foldable.for_ (imports psLines) $ \x' ->
      case Data.HashMap.Strict.lookup x' dependencies of
        Just (src, x) -> transitiveImports buildFile dependencies src x
        Nothing       -> Control.Monad.unless (prim x') (go x')
    Control.Monad.State.Strict.modify'
      (Data.HashMap.Strict.insert module'' (Build.File $ Data.Text.pack fileName'))
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
      (Data.HashMap.Strict.HashMap ModuleName Build.File)
      Development.Shake.Action
      ()
  go module' = do
    let file@(Build.File rawFile) = fileFromModuleName module'
        fileModuleName' = FileModuleName { file, module' }
    fileExists <-
      Control.Monad.Trans.lift
        ( Development.Shake.doesFileExist
          $ Data.Text.unpack src'
          </> Data.Text.unpack rawFile
        )
    if fileExists
      then transitiveImports buildFile dependencies (Build.Dir src') fileModuleName'
      else moduleNotFound module'

fileFromModuleName :: ModuleName -> Build.File
fileFromModuleName (ModuleName name) =
  Build.File
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
