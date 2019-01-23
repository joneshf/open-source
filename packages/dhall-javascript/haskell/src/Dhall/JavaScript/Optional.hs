-- |
-- Module: Dhall.JavaScript.List
-- Description: The logic for compiling Dhall optionals to JavaScript arrays.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
--
-- It's important to note that we compile to JavaScript arrays
-- where there is at most one element in the array.
module Dhall.JavaScript.Optional
  ( build
  , fold
  , literal
  , optional
  ) where

import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "language-ecmascript" Language.ECMAScript3 (Expression)

import qualified "this" CodeGen

-- |
-- @
-- 'build' ~
--   function optionalBuild(_) {
--     return function(f) {
--       var just = function(x) {
--         return [x];
--       };
--
--       return f(null)(just)([]);
--     };
--   }
-- @
build :: (Member Fresh e) => Eff e (Expression ())
build =
  CodeGen.func "optionalBuild" $ \_ ->
    CodeGen.lambda $ \f -> do
      just <- CodeGen.lambda $ \x' -> do
        x <- CodeGen.var x'
        literal (Just x)
      nothing <- literal Nothing
      type' <- optional
      CodeGen.var f
        `CodeGen.call1` type'
        `CodeGen.call1` just
        `CodeGen.call1` nothing

-- |
-- @
-- 'fold' ~
--   function optionalFold(_) {
--     return function(optional) {
--       return function(_) {
--         return function(just) {
--           return function(nothing) {
--             var f = function(_, x) {
--               return just(x);
--             };
--
--             return xs.reduceRight(f, nothing);
--           };
--         };
--       };
--     };
--   }
-- @
fold :: (Member Fresh e) => Eff e (Expression ())
fold =
  CodeGen.func "optionalFold" $ \_ ->
    CodeGen.lambda $ \optional' ->
      CodeGen.lambda $ \_ ->
        CodeGen.lambda $ \just ->
          CodeGen.lambda $ \nothing -> do
            f <- CodeGen.lambda2 $ \(_acc, x) ->
              pure just `CodeGen.call1` x
            optional'
              `CodeGen.property` "reduceRight"
              `CodeGen.call2` (f, nothing)

-- |
-- @
-- 'literal' (Just x) ~ [x]
-- 'literal' Nothing ~ []
-- @
literal :: Maybe (Expression ()) -> Eff e (Expression ())
literal = CodeGen.array

-- |
-- @
-- 'optional' ~
--   function (_) {
--     return null;
--   }
-- @
optional :: (Member Fresh e) => Eff e (Expression ())
optional = CodeGen.lambda (const CodeGen.type')
