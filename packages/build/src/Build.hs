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
module Build where

import qualified "text" Data.Text
import qualified "shake" Development.Shake.Classes
import qualified "unordered-containers" Data.HashMap.Strict
import qualified "dhall" Dhall
import qualified "dhall" Dhall.Core
import qualified "dhall" Dhall.Parser
import qualified "dhall" Dhall.TypeCheck
import qualified "base" GHC.Exts
import qualified "base" GHC.Generics

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

newtype Dir = Dir Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype File = File Data.Text.Text
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

newtype ModuleName = ModuleName Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Name = Name Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype URI = URI Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Version = Version Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

psURIs ::
  (Foldable f) =>
  f Artifact ->
  Data.HashMap.Strict.HashMap (Name, Version) URI
psURIs artifacts = flip foldMap artifacts $ \case
  PureScriptProgram { dependencies } -> flip foldMap dependencies $ \case
    PureScript { name, uri, version } ->
      Data.HashMap.Strict.singleton (name, version) uri

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
