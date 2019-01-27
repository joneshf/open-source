-- |
-- Module: Rename
-- Description: Alpha normalization without using De Bruijn indicies.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Rename
  ( NotRenamed(..)
  , exit
  , failure
  , normalize
  ) where

import "freer-simple" Control.Monad.Freer       (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh (Fresh)
import "text" Data.Text                         (Text)
import "dhall" Dhall.Core                       (Expr, Var)
import "this" Exit                              (Exit)
import "this" Log                               (Log)

import qualified "freer-simple" Control.Monad.Freer
import qualified "text" Data.Text
import qualified "dhall" Dhall.Core
import qualified "this" Exit
import qualified "this" Fresh
import qualified "this" Log

-- |
-- Variables that were not renamed properly.
data NotRenamed a where
  NotRenamed :: Var -> NotRenamed a

-- |
-- If any variables were not renamed,
-- communicate the problem and what to do about it.
exit :: (Member Exit e, Member Log e) => NotRenamed a -> Eff e a
exit = \case
  NotRenamed var -> do
    Log.error
      ( "Haven't renamed: "
      <> Data.Text.pack (show var)
      <> ". This should have been renamed,"
      <> " and is a problem with dhall-javascript."
      <> " Please report this problem."
      )
    Exit.failure

-- |
-- We expect all variables to have been renamed,
-- removing their De Bruijn index in the process.
-- If a variable has not been renamed, we want to know about it.
failure :: (Member NotRenamed e) => Var -> Eff e a
failure = Control.Monad.Freer.send . NotRenamed

-- |
-- Alpha-normalize an expression by renaming each variable with a 'Fresh' name.
--
-- JavaScript does not support referencing shadowed names.
-- If we have @'Var' x 1@, we cannot translate that to JavaScript.
-- We have to rename the variable and the reference.
--
-- We follow the algorithm outlined in v2.0.0 of the
-- [standard](https://github.com/dhall-lang/dhall-lang/blob/v2.0.0/standard/semantics.md#%CE%B1-normalization).
-- There is one important difference:
-- instead of renaming everything to `_` and using De Bruijn indicies,
-- we rename everything with a 'Fresh' name.
normalize :: (Member Fresh e, Member Log e) => Expr a b -> Eff e (Expr a b)
normalize = \case
  Dhall.Core.Annot x y -> Dhall.Core.Annot <$> normalize x <*> normalize y
  Dhall.Core.App x y -> Dhall.Core.App <$> normalize x <*> normalize y
  Dhall.Core.Bool -> pure Dhall.Core.Bool
  Dhall.Core.BoolAnd x y -> Dhall.Core.BoolAnd <$> normalize x <*> normalize y
  Dhall.Core.BoolEQ x y -> Dhall.Core.BoolEQ <$> normalize x <*> normalize y
  Dhall.Core.BoolIf x y z ->
    Dhall.Core.BoolIf <$> normalize x <*> normalize y <*> normalize z
  Dhall.Core.BoolLit x -> pure (Dhall.Core.BoolLit x)
  Dhall.Core.BoolNE x y -> Dhall.Core.BoolNE <$> normalize x <*> normalize y
  Dhall.Core.BoolOr x y -> Dhall.Core.BoolOr <$> normalize x <*> normalize y
  Dhall.Core.Combine x y -> Dhall.Core.Combine <$> normalize x <*> normalize y
  Dhall.Core.CombineTypes x y ->
    Dhall.Core.CombineTypes <$> normalize x <*> normalize y
  Dhall.Core.Const x -> pure (Dhall.Core.Const x)
  Dhall.Core.Constructors x -> Dhall.Core.Constructors <$> normalize x
  Dhall.Core.Double -> pure Dhall.Core.Double
  Dhall.Core.DoubleLit x -> pure (Dhall.Core.DoubleLit x)
  Dhall.Core.DoubleShow -> pure Dhall.Core.DoubleShow
  Dhall.Core.Embed x -> pure (Dhall.Core.Embed x)
  Dhall.Core.Field x y -> Dhall.Core.Field <$> normalize x <*> pure y
  Dhall.Core.ImportAlt x y -> Dhall.Core.ImportAlt <$> normalize x <*> pure y
  Dhall.Core.Integer -> pure Dhall.Core.Integer
  Dhall.Core.IntegerLit x -> pure (Dhall.Core.IntegerLit x)
  Dhall.Core.IntegerShow -> pure Dhall.Core.IntegerShow
  Dhall.Core.IntegerToDouble -> pure Dhall.Core.IntegerToDouble
  Dhall.Core.Lam x0 _A0 b0 -> do
    Log.debug "Renaming in `Lam`"
    _A1 <- normalize _A0
    (x1, b1) <- normalizeBound x0 b0
    pure (Dhall.Core.Lam x1 _A1 b1)
  Dhall.Core.Let x0 _A0 a0 b0 -> do
    Log.debug "Renaming in `Let`"
    a1 <- normalize a0
    _A1 <- traverse normalize _A0
    (x1, b1) <- normalizeBound x0 b0
    pure (Dhall.Core.Let x1 _A1 a1 b1)
  Dhall.Core.List -> pure Dhall.Core.List
  Dhall.Core.ListAppend x y ->
    Dhall.Core.ListAppend <$> normalize x <*> normalize y
  Dhall.Core.ListBuild -> pure Dhall.Core.ListBuild
  Dhall.Core.ListFold -> pure Dhall.Core.ListFold
  Dhall.Core.ListHead -> pure Dhall.Core.ListHead
  Dhall.Core.ListIndexed -> pure Dhall.Core.ListIndexed
  Dhall.Core.ListLast -> pure Dhall.Core.ListLast
  Dhall.Core.ListLength -> pure Dhall.Core.ListLength
  Dhall.Core.ListLit x y ->
    Dhall.Core.ListLit <$> traverse normalize x <*> traverse normalize y
  Dhall.Core.ListReverse -> pure Dhall.Core.ListReverse
  Dhall.Core.Merge x y z ->
    Dhall.Core.Merge <$> normalize x <*> normalize y <*> traverse normalize z
  Dhall.Core.Natural -> pure Dhall.Core.Natural
  Dhall.Core.NaturalBuild -> pure Dhall.Core.NaturalBuild
  Dhall.Core.NaturalEven -> pure Dhall.Core.NaturalEven
  Dhall.Core.NaturalFold -> pure Dhall.Core.NaturalFold
  Dhall.Core.NaturalIsZero -> pure Dhall.Core.NaturalIsZero
  Dhall.Core.NaturalLit x -> pure (Dhall.Core.NaturalLit x)
  Dhall.Core.NaturalOdd -> pure Dhall.Core.NaturalOdd
  Dhall.Core.NaturalPlus x y ->
    Dhall.Core.NaturalPlus <$> normalize x <*> normalize y
  Dhall.Core.NaturalShow -> pure Dhall.Core.NaturalShow
  Dhall.Core.NaturalTimes x y ->
    Dhall.Core.NaturalTimes <$> normalize x <*> normalize y
  Dhall.Core.NaturalToInteger -> pure Dhall.Core.NaturalToInteger
  Dhall.Core.Note x y -> Dhall.Core.Note x <$> normalize y
  Dhall.Core.Optional -> pure Dhall.Core.Optional
  Dhall.Core.OptionalBuild -> pure Dhall.Core.OptionalBuild
  Dhall.Core.OptionalFold -> pure Dhall.Core.OptionalFold
  Dhall.Core.OptionalLit x y ->
    Dhall.Core.OptionalLit <$> normalize x <*> traverse normalize y
  Dhall.Core.Pi x0 _A0 _B0 -> do
    Log.debug "Renaming in `Pi`"
    _A1 <- normalize _A0
    (x1, _B1) <- normalizeBound x0 _B0
    pure (Dhall.Core.Pi x1 _A1 _B1)
  Dhall.Core.Prefer x y -> Dhall.Core.Prefer <$> normalize x <*> normalize y
  Dhall.Core.Project x y -> Dhall.Core.Project <$> normalize x <*> pure y
  Dhall.Core.Record x -> Dhall.Core.Record <$> traverse normalize x
  Dhall.Core.RecordLit x -> Dhall.Core.RecordLit <$> traverse normalize x
  Dhall.Core.Text -> pure Dhall.Core.Text
  Dhall.Core.TextAppend x y ->
    Dhall.Core.TextAppend <$> normalize x <*> normalize y
  Dhall.Core.TextLit x -> pure (Dhall.Core.TextLit x)
  Dhall.Core.Union x -> Dhall.Core.Union <$> traverse normalize x
  Dhall.Core.UnionLit x y z ->
    Dhall.Core.UnionLit x <$> normalize y <*> traverse normalize z
  Dhall.Core.Var x -> pure (Dhall.Core.Var x)

-- |
-- Alpha-normalize a bound variable with a 'Fresh' name.
--
-- This is the only "interesting" rule for normalization.
-- All other rules are traversing the expression.
--
-- We follow the algorithm outlined in v2.0.0 of the
-- [standard](https://github.com/dhall-lang/dhall-lang/blob/v2.0.0/standard/semantics.md#bound-variables-2).
-- There is one important difference:
-- instead of renaming everything to `_` and using De Bruijn indicies,
-- we rename everything with a 'Fresh' name.
normalizeBound ::
  ( Member Fresh e
  , Member Log e
  ) =>
  Text ->
  Expr a b ->
  Eff e (Text, Expr a b)
normalizeBound x0 b0 = do
  x1V@(Dhall.Core.V x1 _) <- Fresh.v
  Log.debug ("Renaming `" <> x0 <> "` to `" <> x1 <> "`")
  let x0V = Dhall.Core.V x0 0
      x1Var = Dhall.Core.Var x1V
      b1 = Dhall.Core.shift 1 x1V b0
      b2 = Dhall.Core.subst x0V x1Var b1
      b3 = Dhall.Core.shift (-1) x0V b2
  b4 <- normalize b3
  pure (x1, b4)
