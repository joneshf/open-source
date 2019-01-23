-- |
-- Module: Dhall.JavaScript.Integer
-- Description: The logic for compiling Dhall integers to JavaScript numbers.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Dhall.JavaScript.Integer
  ( integer
  , literal
  , Dhall.JavaScript.Integer.show
  , toDouble
  ) where

import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "language-ecmascript" Language.ECMAScript3 (Expression)

import qualified "this" CodeGen

-- |
-- @'integer' ~ null@
integer :: Eff e (Expression ())
integer = CodeGen.type'

-- |
-- @'literal' x ~ x@
literal :: Integer -> Eff e (Expression ())
literal = CodeGen.int

-- |
-- @
-- 'show' ~
--   function integerShow(x) {
--     return x.toString();
--   }
-- @
show :: (Member Fresh e) => Eff e (Expression ())
show = CodeGen.func "integerShow" $ \x ->
  x `CodeGen.property` "toString" `CodeGen.call0` ()

-- |
-- @
-- 'toDouble' ~
--   function integerToDouble(x) {
--     return x;
--   }
-- @
toDouble :: (Member Fresh e) => Eff e (Expression ())
toDouble = CodeGen.func "integerToDouble" CodeGen.var
