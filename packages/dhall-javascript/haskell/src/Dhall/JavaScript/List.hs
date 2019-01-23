-- |
-- Module: Dhall.JavaScript.List
-- Description: The logic for compiling Dhall lists to JavaScript arrays.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
--
-- It's important to note that we compile to JavaScript arrays,
-- since the asymptotics of arrays have an effect on performance.
module Dhall.JavaScript.List
  ( append
  , build
  , fold
  , Dhall.JavaScript.List.head
  , indexed
  , Dhall.JavaScript.List.last
  , Dhall.JavaScript.List.length
  , list
  , literal
  , Dhall.JavaScript.List.reverse
  ) where

import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "language-ecmascript" Language.ECMAScript3 (Expression)

import qualified "this" CodeGen

-- |
-- @'append' xs ys ~ xs.concat(ys)@
append :: Expression () -> Expression () -> Eff e (Expression ())
append xs ys = xs `CodeGen.property` "concat" `CodeGen.call1` ys

-- |
-- @
-- 'build' ~
--   function listBuild(_) {
--     return function(f) {
--       var cons = function(x) {
--         return function(xs) {
--           return xs.unshift(x);
--         };
--       };
--
--       return f(null)(cons)([]);
--     };
--   }
-- @
--
-- N.B. The use of (the mutating method) @Array.prototype.unshift@
-- should be safe:
-- we're use a new array and don't expose it from the function until
-- we're done mutating it.
--
-- If this assumption is incorrect, 'build' should be fixed accordingly.
build :: (Member Fresh e) => Eff e (Expression ())
build =
  CodeGen.func "listBuild" $ \_ ->
    CodeGen.lambda $ \f -> do
      cons <- CodeGen.lambda $ \x ->
        CodeGen.lambda $ \xs ->
          xs `CodeGen.property` "unshift" `CodeGen.call1` x
      nil <- literal []
      type' <- list
      CodeGen.var f
        `CodeGen.call1` type'
        `CodeGen.call1` cons
        `CodeGen.call1` nil

-- |
-- @
-- 'fold' ~
--   function listFold(_) {
--     return function(xs) {
--       return function(_) {
--         return function(cons) {
--           return function(nil) {
--             var f = function(acc, x) {
--               return cons(x)(acc);
--             };
--
--             return xs.reduceRight(f, nil);
--           };
--         };
--       };
--     };
--   }
-- @
fold :: (Member Fresh e) => Eff e (Expression ())
fold =
  CodeGen.func "listFold" $ \_ ->
    CodeGen.lambda $ \xs ->
      CodeGen.lambda $ \_ ->
        CodeGen.lambda $ \cons ->
          CodeGen.lambda $ \nil -> do
            f <- CodeGen.lambda2 $ \(acc, x) ->
              pure cons `CodeGen.call1` x `CodeGen.call1` acc
            xs `CodeGen.property` "reduceRight" `CodeGen.call2` (f, nil)

-- |
-- @
-- 'head' ~
--   function listHead(_) {
--     return function(xs) {
--       return xs.slice(0, 1);
--     };
--   }
-- @
--
-- N.B. We rely on the fact that we compile 'Dhall.Core.Optional' values
-- to arrays with at most one element.
-- If we change how we compile 'Dhall.Core.Optional's,
-- we'll have to change 'head'.
head :: (Member Fresh e) => Eff e (Expression ())
head =
  CodeGen.func "listHead" $ \_ ->
    CodeGen.lambda $ \xs -> do
      zero <- CodeGen.int (0 :: Int)
      one <- CodeGen.int (1 :: Int)
      xs `CodeGen.property` "slice" `CodeGen.call2` (zero, one)

-- |
-- @
-- 'indexed' ~
--   function listIndexed(_) {
--     return function(xs) {
--       var index = function(x, i) {
--         return { index: i, value: x };
--       };
--
--       return xs.map(index);
--     };
--   }
-- @
indexed :: (Member Fresh e) => Eff e (Expression ())
indexed =
  CodeGen.func "listIndexed" $ \_ ->
    CodeGen.lambda $ \xs -> do
      index <- CodeGen.lambda2 $ \(x', i') -> do
        x <- CodeGen.var x'
        i <- CodeGen.var i'
        CodeGen.object [(CodeGen.prop "index", i), (CodeGen.prop "value", x)]
      xs `CodeGen.property` "map" `CodeGen.call1` index

-- |
-- @
-- 'last' ~
--   function listLast(_) {
--     return function(xs) {
--       return xs.slice(-1);
--     };
--   }
-- @
--
-- N.B. We rely on the fact that we compile 'Dhall.Core.Optional's
-- to arrays with at most one element.
-- If we change how we compile 'Dhall.Core.Optional's,
-- we'll have to change 'last'.
last :: (Member Fresh e) => Eff e (Expression ())
last =
  CodeGen.func "listLast" $ \_ ->
    CodeGen.lambda $ \xs -> do
      negativeOne <- CodeGen.int (-1 :: Int)
      xs `CodeGen.property` "slice" `CodeGen.call1` negativeOne

-- |
-- @
-- 'length' ~
--   function listLength(_) {
--     return function(xs) {
--       return xs.length;
--     };
--   }
-- @
length :: (Member Fresh e) => Eff e (Expression ())
length =
  CodeGen.func "listLength" $ \_ ->
    CodeGen.lambda $ \xs ->
      xs `CodeGen.property` "length"

-- |
-- @
-- 'list' ~
--   function (_) {
--     return null;
--   }
-- @
list :: (Member Fresh e) => Eff e (Expression ())
list = CodeGen.lambda (const CodeGen.type')

-- |
-- @'literal' xs ~ xs@
literal :: (Foldable f) => f (Expression ()) -> Eff e (Expression ())
literal = CodeGen.array

-- |
-- @
-- 'reverse' ~
--   function listReverse(_) {
--     return function(xs) {
--       return xs.reverse();
--     };
--   }
-- @
reverse :: (Member Fresh e) => Eff e (Expression ())
reverse =
  CodeGen.func "listReverse" $ \_ ->
    CodeGen.lambda $ \xs ->
      xs `CodeGen.property` "reverse" `CodeGen.call0` ()
