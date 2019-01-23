-- |
-- Module: Dhall.JavaScript.Natural
-- Description: The logic for compiling Dhall naturals to JavaScript numbers.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Dhall.JavaScript.Natural
  ( build
  , Dhall.JavaScript.Natural.even
  , fold
  , isZero
  , literal
  , natural
  , Dhall.JavaScript.Natural.odd
  , plus
  , times
  , Dhall.JavaScript.Natural.show
  , Dhall.JavaScript.Natural.toInteger
  ) where

import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "base" GHC.Natural                         (Natural)
import "language-ecmascript" Language.ECMAScript3 (Expression)

import qualified "this" CodeGen

-- |
-- @
-- 'build' ~
--   function naturalBuild(f) {
--     var succ = function(x) {
--       return 1 + x;
--     };
--
--     return f(null)(succ)(0);
--   }
-- @
build :: (Member Fresh e) => Eff e (Expression ())
build = CodeGen.func "naturalBuild" $ \f -> do
  one <- CodeGen.int (1 :: Int)
  succ' <- CodeGen.lambda $ \x' -> do
    x <- CodeGen.var x'
    one `plus` x
  type' <- natural
  zero <- CodeGen.int (0 :: Int)
  CodeGen.var f `CodeGen.call1` type' `CodeGen.call1` succ' `CodeGen.call1` zero

-- |
-- @
-- 'even' ~
--   function naturalEven(x) {
--     return x % 2 === 0;
--   }
-- @
even :: (Member Fresh e) => Eff e (Expression ())
even = CodeGen.func "naturalEven" $ \x' -> do
  x <- CodeGen.var x'
  zero <- CodeGen.int (0 :: Int)
  two <- CodeGen.int (2 :: Int)
  parity <- x `CodeGen.mod` two
  parity `CodeGen.eq` zero

-- |
-- @
-- 'fold' ~
--   function naturalFold(x) {
--     return function(_) {
--       return function(succ) {
--         return function(zero) {
--           var loop = function(natural) {
--             var result = zero;
--
--             for (var i = 0; i < natural; ++i) {
--               result = succ(result);
--             }
--
--             return result;
--           };
--
--           return loop(x);
--         };
--       };
--     };
--   }
-- @
fold :: (Member Fresh e) => Eff e (Expression ())
fold = CodeGen.func "naturalFold" $ \x' ->
  CodeGen.lambda $ \_ ->
    CodeGen.lambda $ \succ' ->
      CodeGen.lambda $ \zero' -> do
        x <- CodeGen.var x'
        zero <- CodeGen.var zero'
        let iife = CodeGen.lambda'' $ \nat' -> do
              i <- CodeGen.var "i"
              nat <- CodeGen.var nat'
              result' <- CodeGen.var "result"
              result <- CodeGen.var succ' `CodeGen.call1` result'
              loop <-
                CodeGen.for
                  ( ("i", CodeGen.int (0 :: Int))
                  , i `CodeGen.lt` nat
                  , CodeGen.preinc "i"
                  )
                  [ CodeGen.assign "result" result
                  ]
              pure
                [ CodeGen.declaration "result" zero
                , loop
                , CodeGen.returns result'
                ]
        iife `CodeGen.call1` x

-- |
-- @
-- 'isZero' ~
--   function naturalIsZero(x) {
--     return x === 0;
--   }
-- @
isZero :: (Member Fresh e) => Eff e (Expression ())
isZero = CodeGen.func "naturalIsZero" $ \x' -> do
  x <- CodeGen.var x'
  zero <- CodeGen.int (0 :: Int)
  x `CodeGen.eq` zero

-- |
-- @'literal' x ~ x@
literal :: Natural -> Eff e (Expression ())
literal = CodeGen.int

-- |
-- @'natural' ~ null@
natural :: Eff e (Expression ())
natural = CodeGen.type'

-- |
-- @
-- 'odd' ~
--   function naturalOdd(x) {
--     return x % 2 === 1;
--   }
-- @
odd :: (Member Fresh e) => Eff e (Expression ())
odd = CodeGen.func "naturalOdd" $ \x' -> do
  x <- CodeGen.var x'
  one <- CodeGen.int (1 :: Int)
  two <- CodeGen.int (2 :: Int)
  parity <- x `CodeGen.mod` two
  parity `CodeGen.eq` one

-- |
-- @'plus' x y ~ x + y@
plus :: Expression () -> Expression () -> Eff e (Expression ())
plus = CodeGen.add

-- |
-- @'times' x y ~ x * y@
times :: Expression () -> Expression () -> Eff e (Expression ())
times = CodeGen.mul

-- |
-- @
-- 'show' ~
--   function naturalShow(x) {
--     return x.toString();
--   }
-- @
show :: (Member Fresh e) => Eff e (Expression ())
show = CodeGen.func "naturalShow" $ \x ->
  x `CodeGen.property` "toString" `CodeGen.call0` ()

-- |
-- @
-- 'toInteger' ~
--   function naturalToInteger(x) {
--     return x;
--   }
-- @
toInteger :: (Member Fresh e) => Eff e (Expression ())
toInteger = CodeGen.func "naturalToInteger" CodeGen.var
