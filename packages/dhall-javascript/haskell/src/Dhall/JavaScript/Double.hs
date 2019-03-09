-- |
-- Module: Dhall.JavaScript.Double
-- Description: The logic for compiling Dhall doubles to JavaScript numbers.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Dhall.JavaScript.Double
  ( double
  , literal
  , Dhall.JavaScript.Double.show
  ) where

import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "language-ecmascript" Language.ECMAScript3 (Expression)

import qualified "this" CodeGen

-- |
-- @'double' ~ null@
double :: Eff e (Expression ())
double = CodeGen.type'

-- |
-- @'literal' x ~ x@
literal :: Double -> Eff e (Expression ())
literal = CodeGen.number

-- |
-- @
-- 'show' ~
--   function doubleShow(x) {
--     return x.toString();
--   }
-- @
show :: (Member Fresh e) => Eff e (Expression ())
show = CodeGen.func "doubleShow" $ \x ->
  x `CodeGen.property` "toString" `CodeGen.call0` ()
