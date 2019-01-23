-- |
-- Module: Fresh
-- Description: An effect for generating 'Fresh' values.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
--
-- Very thin wrapper around 'Fresh'.
-- Provides helpers to make working with Dhall and JavaScript a bit easier.
module Fresh
  ( eval
  , ident
  , v
  ) where

import "freer-simple" Control.Monad.Freer                        (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh                  (Fresh)
import "text" Data.Text                                          (Text)
import "dhall" Dhall.Core                                        (Var)
import "language-ecmascript" Language.ECMAScript3                (Id)
import "language-ecmascript" Language.ECMAScript3.Syntax.CodeGen ()

import qualified "freer-simple" Control.Monad.Freer.Fresh
import qualified "text" Data.Text
import qualified "dhall" Dhall.Core
import qualified "base" GHC.Exts

-- |
-- Evaluate a 'Fresh' effect starting at 0.
eval :: Eff (Fresh ': e) a -> Eff e a
eval = Control.Monad.Freer.Fresh.evalFresh 0

-- |
-- Generate an 'Id' with a 'Fresh' name.
ident :: (Member Fresh e) => Eff e (Id ())
ident = GHC.Exts.fromString . Data.Text.unpack <$> text

text :: (Member Fresh e) => Eff e Text
text = do
  n <- Control.Monad.Freer.Fresh.fresh
  pure ("$dhall" <> Data.Text.pack (show n))

-- |
-- Generate a 'Var' with a 'Fresh' name.
v :: (Member Fresh e) => Eff e Var
v = do
  x <- text
  pure (Dhall.Core.V x 0)
