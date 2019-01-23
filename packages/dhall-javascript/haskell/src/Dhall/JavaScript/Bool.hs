-- |
-- Module: Dhall.JavaScript.Bool
-- Description: The logic for compiling Dhall booleans to JavaScript booleans.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Dhall.JavaScript.Bool
  ( Dhall.JavaScript.Bool.and
  , bool
  , eq
  , literal
  , neq
  , Dhall.JavaScript.Bool.or
  , ternary
  ) where

import "freer-simple" Control.Monad.Freer         (Eff)
import "language-ecmascript" Language.ECMAScript3 (Expression)

import qualified "this" CodeGen

-- |
-- @'and' x y ~ x && y@
and :: Expression () -> Expression () -> Eff e (Expression ())
and = CodeGen.and

-- |
-- @'bool' ~ null@
bool :: Eff e (Expression ())
bool = CodeGen.type'

-- |
-- @'eq' x y ~ x === y@
eq :: Expression () -> Expression () -> Eff e (Expression ())
eq = CodeGen.eq

-- |
-- @
-- 'literal' True ~ true
-- 'literal' False ~ false
-- @
literal :: Bool -> Eff e (Expression ())
literal = CodeGen.bool

-- |
-- @'neq' x y ~ x !== y@
neq :: Expression () -> Expression () -> Eff e (Expression ())
neq = CodeGen.neq

-- |
-- @'or' x y ~ x || y@
or :: Expression () -> Expression () -> Eff e (Expression ())
or = CodeGen.or

-- |
-- @'ternary' x y z ~ x ? y : z@
ternary ::
  Expression () ->
  Expression () ->
  Expression () ->
  Eff e (Expression ())
ternary = CodeGen.ternary
