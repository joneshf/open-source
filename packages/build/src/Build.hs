{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Build
  ( Dir(Dir)
  , File(File)
  , Name(Name)
  , Platform(Linux)
  , URI(URI)
  , Version(Version)
  ) where

import qualified "text" Data.Text
import qualified "shake" Development.Shake.Classes

newtype Dir = Dir Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype File = File Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Name = Name Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

data Platform
  = Linux

newtype URI = URI Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)

newtype Version = Version Data.Text.Text
  deriving (Development.Shake.Classes.Hashable, Eq)
