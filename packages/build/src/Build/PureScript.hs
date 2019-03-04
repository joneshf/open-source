{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Build.PureScript where

import "shake" Development.Shake          ((%>))
import "shake" Development.Shake.FilePath ((<.>), (</>))

import qualified "this" Build
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
import qualified "shake" Development.Shake.FilePath

rules ::
  Traversable f =>
  f Build.Artifact ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  Data.HashMap.Strict.HashMap (Build.Name, Build.Version) Build.URI ->
  Development.Shake.Rules (f Build.Name)
rules artifacts binDir buildDir buildFile dependenciesDir downloadDir platform psURIs = do
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
        (Data.HashMap.Strict.lookup (Build.Name name, Build.Version version) psURIs)
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
    Build.PureScriptProgram
      { Build.compiler
      , Build.dependencies = dependencies'
      , Build.main' = main'@Build.FileModuleName { module' = module'' }
      , Build.name = Build.Name name'
      , Build.src = src'
      } -> do

        let dependencies ::
              Data.HashMap.Strict.HashMap Build.ModuleName (Build.Dir, Build.FileModuleName)
            dependencies = flip foldMap dependencies' $ \case
              Build.PureScript
                { modules
                , name = Build.Name name
                , src = Build.Dir src
                , version = Build.Version version
                } ->
                  flip foldMap modules $ \x@Build.FileModuleName { module' } ->
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
          modules' <-
            Control.Monad.State.Strict.execStateT
              (transitiveImports buildFile dependencies src' main')
              mempty
          let modules =
                Data.HashSet.fromList (Data.HashMap.Strict.elems modules')
          compile binDir buildDir compiler modules module'' out

        pure (Build.Name name')

compile ::
  FilePath ->
  FilePath ->
  Build.Version ->
  Data.HashSet.HashSet Build.File ->
  Build.ModuleName ->
  FilePath ->
  Development.Shake.Action ()
compile bin buildDir (Build.Version compiler) modules (Build.ModuleName module') out = do
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
  psFiles = Data.HashSet.map (\(Build.File f) -> Data.Text.unpack f) modules
  purs = bin </> "purs" </> Data.Text.unpack compiler </> "purs"
  output = buildDir </> "PureScript/output" </> Data.Text.unpack compiler

transitiveImports ::
  FilePath ->
  Data.HashMap.Strict.HashMap Build.ModuleName (Build.Dir, Build.FileModuleName) ->
  Build.Dir ->
  Build.FileModuleName ->
  Control.Monad.State.Strict.StateT
    (Data.HashMap.Strict.HashMap Build.ModuleName Build.File)
    Development.Shake.Action
    ()
transitiveImports buildFile dependencies (Build.Dir src') Build.FileModuleName { file = Build.File file', module' = module'' } = do
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

  moduleNotFound :: (Control.Monad.Fail.MonadFail f) => Build.ModuleName -> f a
  moduleNotFound (Build.ModuleName name) =
    fail
      ( unwords
        [ "Could not find module"
        , show name <> "."
        , "Please add the dependency to"
        , show buildFile
        ]
      )

  go ::
    Build.ModuleName ->
    Control.Monad.State.Strict.StateT
      (Data.HashMap.Strict.HashMap Build.ModuleName Build.File)
      Development.Shake.Action
      ()
  go module' = do
    let file@(Build.File rawFile) = fileFromModuleName module'
        fileModuleName' = Build.FileModuleName { file, module' }
    fileExists <-
      Control.Monad.Trans.lift
        ( Development.Shake.doesFileExist
          $ Data.Text.unpack src'
          </> Data.Text.unpack rawFile
        )
    if fileExists
      then transitiveImports buildFile dependencies (Build.Dir src') fileModuleName'
      else moduleNotFound module'

fileFromModuleName :: Build.ModuleName -> Build.File
fileFromModuleName (Build.ModuleName name) =
  Build.File
    $ Data.Text.pack
    $ Development.Shake.FilePath.joinPath
      (Data.Text.unpack <$> Data.Text.splitOn (Data.Text.pack ".") name)
    <.> "purs"

imports :: [String] -> Data.HashSet.HashSet Build.ModuleName
imports =
  Data.HashSet.fromList . Data.Maybe.mapMaybe (parsePrefix "import")

parsePrefix :: String -> String -> Maybe Build.ModuleName
parsePrefix needle' haystack = case words haystack of
  (needle:name:_) | needle == needle' -> pure (Build.ModuleName $ Data.Text.pack name)
  _                                   -> Nothing

prim :: Build.ModuleName -> Bool
prim = (`Data.HashSet.member` primModules)

primModules :: Data.HashSet.HashSet Build.ModuleName
primModules =
  Data.HashSet.fromList
    [ Build.ModuleName (Data.Text.pack "Prim")
    , Build.ModuleName (Data.Text.pack "Prim.Boolean")
    , Build.ModuleName (Data.Text.pack "Prim.Ordering")
    , Build.ModuleName (Data.Text.pack "Prim.Row")
    , Build.ModuleName (Data.Text.pack "Prim.RowList")
    , Build.ModuleName (Data.Text.pack "Prim.Symbol")
    , Build.ModuleName (Data.Text.pack "Prim.TypeError")
    ]
