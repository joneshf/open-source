-- |
-- Module: Dhall.JavaScript.Record
-- Description: The logic for compiling Dhall records to JavaScript objects.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Dhall.JavaScript.Record
  ( combine
  , combineTypes
  , field
  , literal
  , prefer
  , project
  , record
  ) where

import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "text" Data.Text                           (Text)
import "language-ecmascript" Language.ECMAScript3 (Expression, Id, Prop)

import qualified "this" CodeGen

-- |
-- @
-- 'combine' x y ~
--   (function combine(source, target) {
--     var result = Object.assign({}, source);
--
--     for (var key in target) {
--       if (target.hasOwnProperty(key)) {
--         if ({}.toString.call(target[key]) === '[object Object]' &&
--             {}.toString.call(result[key]) === '[object Object]') {
--           result[key] = combine(result[key], target[key]);
--         } else {
--           result[key] = target[key];
--         }
--       }
--     }
--
--     return result;
--   })(x, y)
-- @
combine ::
  (Member Fresh e) =>
  Expression () ->
  Expression () ->
  Eff e (Expression ())
combine old new = iife `CodeGen.call2` (old, new)
  where
  iife = CodeGen.func2 "combine" $ \(source', target') -> do
    _Object <- CodeGen.var "Object"
    object <- CodeGen.object []
    key <- CodeGen.var "key"
    source <- CodeGen.var source'
    sourceVal <- source `CodeGen.brack` key
    sourceObject <- isObject sourceVal
    target <- CodeGen.var target'
    targetVal <- target `CodeGen.brack` key
    targetObject <- isObject targetVal
    bothObjects <- targetObject `CodeGen.and` sourceObject
    hasProperty <-
      target `CodeGen.property` "hasOwnProperty" `CodeGen.call1` key
    result <- CodeGen.var "result"
    resultValue <-
      _Object `CodeGen.property` "assign" `CodeGen.call2` (object, source)
    deep <- CodeGen.var "combine" `CodeGen.call2` (result, target)
    loop <-
      CodeGen.forin
        ("key", pure target)
        [ CodeGen.ift
          hasProperty
          [ CodeGen.ifte
            bothObjects
            [ CodeGen.assign' result key deep
            ]
            [ CodeGen.assign' result key targetVal
            ]
          ]
        ]
    pure
      [ CodeGen.declaration "result" resultValue
      , loop
      , CodeGen.returns result
      ]

  isObject x = do
    object <- CodeGen.object []
    objectObject <- CodeGen.string "[object Object]"
    toString <- object `CodeGen.property` "toString"
    type' <- toString `CodeGen.property` "call" `CodeGen.call1` x
    CodeGen.eq type' objectObject

-- |
-- @'combineTypes' x y ~ null@
combineTypes :: Expression () -> Expression () -> Eff e (Expression ())
combineTypes _ _ = record

-- |
-- @'field' x y ~ x.y@
--
-- if @y@ is a valid JavaScript identifier.
--
-- @'field' x y ~ x[y]@
--
-- if @y@ is not a valid JavaScript identifier.
field :: Expression () -> Text -> Eff e (Expression ())
field = CodeGen.property

-- |
-- @'literal' [(k1, v1), (k2, v2), ...] ~ {k1: v1, k2: v2, ...}@
--
-- N.B. Will escape keys according to the rules of 'field'.
literal :: (Foldable f) => f (Prop (), Expression ()) -> Eff e (Expression ())
literal = CodeGen.object

-- |
-- @'prefer' x y ~ Object.assign({}, x, y)@
prefer :: Expression () -> Expression () -> Eff e (Expression ())
prefer old new = do
  object <- CodeGen.object []
  _Object <- CodeGen.var "Object"
  _Object `CodeGen.property` "assign" `CodeGen.call3` (object, old, new)

-- |
-- @
-- 'project' x f ~
--   (function recordProject(object) {
--     return {
--       field1: object.field1,
--       field2: object.field2,
--       ...,
--       fieldN: object.fieldN,
--     };
--   })(x)
-- @
--
-- where @object@ is a 'Fresh' 'Id',
-- and each @field1: object.field1@ through @fieldN: object.fieldN@
-- are the result of @f object@.
project ::
  ( Foldable f
  , Member Fresh e
  ) =>
  Expression () ->
  (Id () -> Eff e (f (Prop (), Expression ()))) ->
  Eff e (Expression ())
project expression toPairs = do
  let iife = CodeGen.func "recordProject" $ \var -> do
        pairs <- toPairs var
        CodeGen.object pairs
  iife `CodeGen.call1` expression

-- |
-- @'record' ~ null@
record :: Eff e (Expression ())
record = CodeGen.type'
