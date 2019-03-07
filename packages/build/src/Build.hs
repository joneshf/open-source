{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Build
  ( Dir(Dir)
  , File(File)
  , Name(Name)
  , URI(URI)
  , Version(Version)
  , union
  ) where

import qualified "text" Data.Text
import qualified "shake" Development.Shake.Classes
import qualified "dhall" Dhall
import qualified "dhall" Dhall.Core
import qualified "dhall" Dhall.Parser
import qualified "dhall" Dhall.TypeCheck
import qualified "base" GHC.Exts

newtype Dir = Dir Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype File = File Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Name = Name Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype URI = URI Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Version = Version Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

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
