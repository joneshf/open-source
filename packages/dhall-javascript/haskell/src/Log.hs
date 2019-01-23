-- |
-- Module: Log
-- Description: An effect for 'Log'ging from the program.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Log
  ( Log(..)
  , Logger
  , Severity(..)
  , debug
  , Log.error
  , io
  ) where

import "base" Control.Monad               (when)
import "freer-simple" Control.Monad.Freer (Eff, Member)
import "text" Data.Text                   (Text)

import qualified "freer-simple" Control.Monad.Freer
import qualified "text" Data.Text
import qualified "text" Data.Text.IO
import qualified "base" System.Environment
import qualified "base" System.IO

-- |
-- Effect to capture 'Log'ging.
data Log a where
  Log :: Severity -> Text -> Log ()

-- |
-- Synonym to clean up type signatures.
type Logger f = forall a. Log a -> f a

-- |
-- The level of severity to log
data Severity
  = Debug
  | Error
  deriving (Eq, Ord)

-- |
-- Helper to make logging with 'Debug' severity easier.
debug :: (Member Log e) => Text -> Eff e ()
debug = Control.Monad.Freer.send . Log Debug

-- |
-- Helper to make logging with 'Debug' severity easier.
error :: (Member Log e) => Text -> Eff e ()
error = Control.Monad.Freer.send . Log Error

-- |
-- Interpret the 'Log' effect to 'IO'.
-- Will print output to STDERR.
io :: Severity -> Logger IO
io minimum' = \case
  Log severity msg -> when (severity >= minimum') $ do
    name <- Data.Text.pack <$> System.Environment.getProgName
    Data.Text.IO.hPutStrLn
      System.IO.stderr
      (name <> ": " <> render severity <> " " <> msg)

render :: Severity -> Text
render = \case
  Debug -> "[DEBUG]"
  Error -> "[ERROR]"
