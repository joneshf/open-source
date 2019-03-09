-- |
-- Module: Dhall.JavaScript.Text
-- Description: The logic for compiling Dhall texts to JavaScript strings.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Dhall.JavaScript.Text
  ( append
  , literal
  , Dhall.JavaScript.Text.show
  , text
  ) where

import "freer-simple" Control.Monad.Freer         (Eff)
import "text" Data.Text                           (Text)
import "language-ecmascript" Language.ECMAScript3 (Expression)

import qualified "this" CodeGen
import qualified "base" Control.Monad

-- |
-- @'append' x y ~ x + y@
append :: Expression () -> Expression () -> Eff e (Expression ())
append = CodeGen.add

-- |
-- @'literal' [(t1, x1), (t2, x2), ...] tn ~ t1 + x1 + t2 + x2 + ... + tn@
literal ::
  (Foldable f) =>
  f (Text, Expression ()) ->
  Text ->
  Eff e (Expression ())
literal xs x' = do
  x <- CodeGen.string x'
  Control.Monad.foldM go x xs
  where
  go :: Expression () -> (Text, Expression ()) -> Eff e (Expression ())
  go acc (text'', expression) = do
    text' <- CodeGen.string text''
    temp <- text' `CodeGen.add` expression
    temp `CodeGen.add` acc

-- |
-- @'show' ~ JSON.stringify@
show :: Eff e (Expression ())
show = do
  _JSON <- CodeGen.var "JSON"
  _JSON `CodeGen.property` "stringify"

-- |
-- @'text' ~ null@
text :: Eff e (Expression ())
text = CodeGen.type'
