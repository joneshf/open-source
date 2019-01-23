-- |
-- Module: Dhall.JavaScript.Union
-- Description: The logic for compiling Dhall unions to JavaScript functions.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
--
-- Since Dhall unions are ad-hoc first-class values,
-- and the only thing you can do with them is merge them,
-- we compile to functions.
--
-- Each function closes over the value of the union.
-- When 'merge'd, it will invoke the appropriate field with the union value.
--
-- This is effectively Church encoding each alternate.
module Dhall.JavaScript.Union
  ( constructors
  , literal
  , merge
  , union
  ) where

import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "text" Data.Text                           (Text)
import "language-ecmascript" Language.ECMAScript3 (Expression)

import qualified "this" CodeGen
import qualified "text" Data.Text
import qualified "base" Data.Traversable
import qualified "base" GHC.Exts

-- |
-- @'constructors' [(k1, v1), (k2, v2), ...] ~ {k1: v1, k2: v2, ...}@
constructors ::
  ( Member Fresh e
  , Traversable f
  ) =>
  f (Text, Expression ()) ->
  Eff e (Expression ())
constructors xs' = do
  xs <- Data.Traversable.for xs' $ \(label, expression') -> do
    expression <- literal label expression'
    pure (CodeGen.prop label, expression)
  CodeGen.object xs

-- |
-- @
-- 'literal' x y ~
--   function(object) {
--     return object.x(y);
--   },
-- @
literal :: (Member Fresh e) => Text -> Expression () -> Eff e (Expression ())
literal label expression = do
  let alternative = GHC.Exts.fromString (Data.Text.unpack label)
  CodeGen.lambda $ \object ->
    object `CodeGen.property` alternative `CodeGen.call1` expression

-- |
-- @'merge' x y ~ y(x)@
merge :: Expression () -> Expression () -> Eff e (Expression ())
merge record union' =
  pure union' `CodeGen.call1` record

-- |
-- @'union' ~ null@
union :: Eff e (Expression ())
union = CodeGen.type'
