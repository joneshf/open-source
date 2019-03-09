-- |
-- Module: Dhall.JavaScript.Core
-- Description: The core of the logic for compiling to JavaScript.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Dhall.JavaScript.Core
  ( toJS
  ) where

import "base" Control.Monad                       ((>=>))
import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "base" Data.List.NonEmpty                  (NonEmpty((:|)))
import "text" Data.Text                           (Text)
import "dhall" Dhall.Core                         (Expr)
import "dhall" Dhall.TypeCheck                    (X)
import "this" Exit                                (Exit)
import "language-ecmascript" Language.ECMAScript3 (Expression, Id, Prop)
import "this" Log                                 (Log)
import "this" Rename                              (NotRenamed)

import qualified "this" CodeGen
import qualified "text" Data.Text
import qualified "dhall" Dhall.Core
import qualified "this" Dhall.JavaScript.Bool
import qualified "this" Dhall.JavaScript.Double
import qualified "this" Dhall.JavaScript.Integer
import qualified "this" Dhall.JavaScript.List
import qualified "this" Dhall.JavaScript.Natural
import qualified "this" Dhall.JavaScript.Optional
import qualified "this" Dhall.JavaScript.Record
import qualified "this" Dhall.JavaScript.Text
import qualified "this" Dhall.JavaScript.Union
import qualified "dhall" Dhall.Set
import qualified "dhall" Dhall.TypeCheck
import qualified "this" Exit
import qualified "base" GHC.Exts
import qualified "language-ecmascript" Language.ECMAScript3
import qualified "this" Log
import qualified "this" Rename

-- |
-- Convert a Dhall expression to a JavaScript expression
toJS ::
  forall e.
  ( Member Fresh e
  , Member Exit e
  , Member Log e
  , Member NotRenamed e
  ) =>
  Expr X X ->
  Eff e (Expression ())
toJS = Rename.normalize >=> go
  where
  go :: Expr X X -> Eff e (Expression ())
  go x = do
    Log.debug ("In `toJS` with Expr: " <> Data.Text.pack (show x))
    go' x
  go' :: Expr X X -> Eff e (Expression ())
  go' = \case
    Dhall.Core.Annot _note x -> go x
    Dhall.Core.App f x' -> do
      x <- go x'
      go f `CodeGen.call1` x
    Dhall.Core.Bool -> Dhall.JavaScript.Bool.bool
    Dhall.Core.BoolAnd x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Bool.and x y
    Dhall.Core.BoolEQ x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Bool.eq x y
    Dhall.Core.BoolIf x' y' z' -> do
      x <- go x'
      y <- go y'
      z <- go z'
      Dhall.JavaScript.Bool.ternary x y z
    Dhall.Core.BoolNE x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Bool.neq x y
    Dhall.Core.BoolOr x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Bool.or x y
    Dhall.Core.BoolLit b -> Dhall.JavaScript.Bool.literal b
    Dhall.Core.Combine x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Record.combine x y
    Dhall.Core.CombineTypes x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Record.combineTypes x y
    Dhall.Core.Const Dhall.Core.Kind -> CodeGen.kind
    Dhall.Core.Const Dhall.Core.Sort -> CodeGen.sort
    Dhall.Core.Const Dhall.Core.Type -> CodeGen.type'
    Dhall.Core.Double -> Dhall.JavaScript.Double.double
    Dhall.Core.DoubleLit x -> Dhall.JavaScript.Double.literal x
    Dhall.Core.DoubleShow -> Dhall.JavaScript.Double.show
    Dhall.Core.Embed x -> Dhall.TypeCheck.absurd x
    Dhall.Core.Field expression' key -> do
      expression <- go expression'
      Dhall.JavaScript.Record.field expression key
    Dhall.Core.ImportAlt x y -> do
      Log.error
        ( "ImportAlt was not resolved: "
        <> " import: "
        <> Data.Text.pack (show x)
        <> " alternative: "
        <> Data.Text.pack (show y)
        )
      Exit.failure
    Dhall.Core.Integer -> Dhall.JavaScript.Integer.integer
    Dhall.Core.IntegerLit x -> Dhall.JavaScript.Integer.literal x
    Dhall.Core.IntegerShow -> Dhall.JavaScript.Integer.show
    Dhall.Core.IntegerToDouble -> Dhall.JavaScript.Integer.toDouble
    Dhall.Core.Note _ x -> go x
    Dhall.Core.Optional -> Dhall.JavaScript.Optional.optional
    Dhall.Core.OptionalBuild -> Dhall.JavaScript.Optional.build
    Dhall.Core.OptionalFold -> Dhall.JavaScript.Optional.fold
    Dhall.Core.OptionalLit _type xs' -> do
      xs <- traverse go xs'
      Dhall.JavaScript.Optional.literal xs
    Dhall.Core.Lam argument _type body ->
      CodeGen.lambda' (GHC.Exts.fromString $ Data.Text.unpack argument) $
        go body
    Dhall.Core.Let (Dhall.Core.Binding x Nothing y' :| []) z -> do
      y <- go y'
      CodeGen.lambda' (GHC.Exts.fromString $ Data.Text.unpack x) (go z)
        `CodeGen.call1` y
    Dhall.Core.Let (Dhall.Core.Binding w Nothing x' :| y : ys) z -> do
      x <- go x'
      CodeGen.lambda'
        (GHC.Exts.fromString $ Data.Text.unpack w)
        (go $ Dhall.Core.Let (y :| ys) z)
          `CodeGen.call1` x
    Dhall.Core.Let (Dhall.Core.Binding w (Just x) y :| []) z ->
      go (Dhall.Core.normalize $ Dhall.Core.App (Dhall.Core.Lam w x z) y)
    Dhall.Core.Let (Dhall.Core.Binding v (Just w) x :| y : ys) z' ->
      go
        ( Dhall.Core.normalize
        $ Dhall.Core.App (Dhall.Core.Lam v w $ Dhall.Core.Let (y :| ys) z') x
        )
    Dhall.Core.List -> Dhall.JavaScript.List.list
    Dhall.Core.ListAppend xs' ys' -> do
      xs <- go xs'
      ys <- go ys'
      Dhall.JavaScript.List.append xs ys
    Dhall.Core.ListBuild -> Dhall.JavaScript.List.build
    Dhall.Core.ListFold -> Dhall.JavaScript.List.fold
    Dhall.Core.ListHead -> Dhall.JavaScript.List.head
    Dhall.Core.ListIndexed -> Dhall.JavaScript.List.indexed
    Dhall.Core.ListLast -> Dhall.JavaScript.List.last
    Dhall.Core.ListLength -> Dhall.JavaScript.List.length
    Dhall.Core.ListLit _type xs' -> do
      xs <- traverse go xs'
      Dhall.JavaScript.List.literal xs
    Dhall.Core.ListReverse -> Dhall.JavaScript.List.reverse
    Dhall.Core.Merge record' union' _type -> do
      record <- go record'
      union <- go union'
      Dhall.JavaScript.Union.merge record union
    Dhall.Core.Natural -> Dhall.JavaScript.Natural.natural
    Dhall.Core.NaturalBuild -> Dhall.JavaScript.Natural.build
    Dhall.Core.NaturalEven -> Dhall.JavaScript.Natural.even
    Dhall.Core.NaturalFold -> Dhall.JavaScript.Natural.fold
    Dhall.Core.NaturalIsZero -> Dhall.JavaScript.Natural.isZero
    Dhall.Core.NaturalLit n -> Dhall.JavaScript.Natural.literal n
    Dhall.Core.NaturalOdd -> Dhall.JavaScript.Natural.odd
    Dhall.Core.NaturalPlus x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Natural.plus x y
    Dhall.Core.NaturalShow -> Dhall.JavaScript.Natural.show
    Dhall.Core.NaturalTimes x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Natural.times x y
    Dhall.Core.NaturalToInteger -> Dhall.JavaScript.Natural.toInteger
    Dhall.Core.None -> Dhall.JavaScript.Optional.literal Nothing
    Dhall.Core.Pi argument _type body ->
      CodeGen.lambda' (GHC.Exts.fromString $ Data.Text.unpack argument) $
        go body
    Dhall.Core.Prefer old' new' -> do
      old <- go old'
      new <- go new'
      Dhall.JavaScript.Record.prefer old new
    Dhall.Core.Project expression' fields -> do
      expression <- go expression'
      Dhall.JavaScript.Record.project expression $ \var ->
        traverse (toObjectPair . toPair var) (Dhall.Set.toList fields)
    Dhall.Core.Record _type -> Dhall.JavaScript.Record.record
    Dhall.Core.RecordLit pairs' -> do
      pairs <- traverse toObjectPair (GHC.Exts.toList pairs')
      Dhall.JavaScript.Record.literal pairs
    Dhall.Core.Some x' -> do
      x <- go x'
      Dhall.JavaScript.Optional.literal (Just x)
    Dhall.Core.Text -> Dhall.JavaScript.Text.text
    Dhall.Core.TextAppend x' y' -> do
      x <- go x'
      y <- go y'
      Dhall.JavaScript.Text.append x y
    Dhall.Core.TextLit (Dhall.Core.Chunks xs' x) -> do
      xs <- (traverse . traverse) go xs'
      Dhall.JavaScript.Text.literal xs x
    Dhall.Core.TextShow -> Dhall.JavaScript.Text.show
    Dhall.Core.Union _type -> Dhall.JavaScript.Union.union
    Dhall.Core.UnionLit label expression' _alternatives -> do
      expression <- go expression'
      Dhall.JavaScript.Union.literal label expression
    Dhall.Core.Var (Dhall.Core.V variable 0) ->
      CodeGen.var (GHC.Exts.fromString $ Data.Text.unpack variable)
    Dhall.Core.Var x -> Rename.failure x
  toObjectPair :: (Text, Expr X X) -> Eff e (Prop (), Expression ())
  toObjectPair = \case
    (key, val') -> do
      val <- go' val'
      pure (CodeGen.prop key, val)
  toPair :: Id () -> Text -> (Text, Expr a b)
  toPair (Language.ECMAScript3.Id _ var) field =
    (field, Dhall.Core.Field (Dhall.Core.Var $ GHC.Exts.fromString var) field)
