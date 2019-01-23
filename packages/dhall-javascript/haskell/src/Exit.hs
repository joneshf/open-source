-- |
-- Module: Exit
-- Description: An effect for 'Exit'ing the program.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Exit
  ( Exit(..)
  , Exiter
  , die
  , failure
  , io
  ) where

import "freer-simple" Control.Monad.Freer (Eff, Member)
import "text" Data.Text                   (Text)

import qualified "freer-simple" Control.Monad.Freer
import qualified "text" Data.Text
import qualified "base" System.Exit

-- |
-- How to exit the program.
data Exit a where
  Die :: Text -> Exit a
  Failure :: Int -> Exit a

-- |
-- Synonym to clean up type signatures
type Exiter f = forall a. Exit a -> f a

-- |
-- Interpret the exit effect to 'IO'.
io :: Exiter IO
io = \case
  Die msg -> System.Exit.die (Data.Text.unpack msg)
  Failure code -> System.Exit.exitWith (System.Exit.ExitFailure code)

-- |
-- Helper to make exiting with a 'Die' easier.
die :: (Member Exit e) => Text -> Eff e a
die = Control.Monad.Freer.send . Die

-- |
-- Helper to make exiting with a 'Failure' easier.
failure :: (Member Exit e) => Eff e a
failure = Control.Monad.Freer.send (Failure 1)
