-- |
-- Module: Render
-- Description: Render a Dhall expression in different ways.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Render
  ( expression
  , module'
  , variable
  ) where

import "freer-simple" Control.Monad.Freer                     (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh               (Fresh)
import "text" Data.Text                                       (Text)
import "dhall" Dhall.Core                                     (Expr)
import "dhall" Dhall.TypeCheck                                (X)
import "this" Exit                                            (Exit, Exiter)
import "language-ecmascript" Language.ECMAScript3             (Expression)
import "language-ecmascript" Language.ECMAScript3.PrettyPrint (Pretty)
import "this" Log                                             (Log, Logger)
import "this" Rename                                          (NotRenamed)

import qualified "this" CodeGen
import qualified "freer-simple" Control.Monad.Freer
import qualified "text" Data.Text
import qualified "this" Dhall.JavaScript.Core
import qualified "this" Fresh
import qualified "language-ecmascript" Language.ECMAScript3.PrettyPrint
import qualified "this" Log
import qualified "this" Rename
import qualified "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen

eval ::
  (Monad f) =>
  Exiter f ->
  Logger f ->
  Eff
    '[ NotRenamed
     , Fresh
     , Exit
     , Log
     , f
     ]
    a ->
  f a
eval exiter logger =
  Control.Monad.Freer.runM
    . Control.Monad.Freer.interpretM logger
    . Control.Monad.Freer.interpretM exiter
    . Fresh.eval
    . Control.Monad.Freer.interpret Rename.exit

-- |
-- Render a Dhall expression as a JavaScript expression.
-- Most useful when building up a larger program.
--
-- E.g. if you called: @'expression' ('Dhall.Core.BoolLit' 'False')@,
-- it would be rendered as: @false@
expression :: (Monad f) => Exiter f -> Logger f -> Expr X X -> f Text
expression exiter logger expr' = eval exiter logger $ do
  expr <- toJS expr'
  pure (render expr)

-- |
-- Render a Dhall expression as a CommonJS JavaScript module
-- Most useful for using as a stand-alone JavaScript program.
-- The expression itself is the export.
-- Can be @require@d from node or browserified or whatever else you can do
-- with CommonJS modules.
--
-- E.g. if you had: @'module'' ('Dhall.Core.BoolLit' 'False')@,
-- it would be rendered as: @module.exports = false;@
module' :: (Monad f) => Exiter f -> Logger f -> Expr X X -> f Text
module' exiter logger expr' = eval exiter logger $ do
  expr <- toJS expr'
  module'' <- CodeGen.module' expr
  Log.debug ("JavaScript Module: " <> Data.Text.pack (show module''))
  pure (render module'')

render :: (Pretty a) => a -> Text
render x =
  Data.Text.pack
    $ Text.PrettyPrint.ANSI.Leijen.displayS
      ( Text.PrettyPrint.ANSI.Leijen.renderSmart 1 80
      $ Language.ECMAScript3.PrettyPrint.prettyPrint x
      )
      ""

toJS ::
  ( Member Fresh e
  , Member Exit e
  , Member Log e
  , Member NotRenamed e
  ) =>
  Expr X X ->
  Eff e (Expression ())
toJS expr' = do
  Log.debug ("Dhall Expr: " <> Data.Text.pack (show expr'))
  expr <- Dhall.JavaScript.Core.toJS expr'
  Log.debug ("JavaScript Expression: " <> Data.Text.pack (show expr))
  pure expr

-- |
-- Render a Dhall expression as a JavaScript variable declaration.
-- Most useful for building up a larger JavaScript program.
--
-- E.g. if you had: @'variable' "foo" ('Dhall.Core.BoolLit' 'False')@,
-- it would be rendered as: @var foo = false;@
variable :: (Monad f) => Exiter f -> Logger f -> Text -> Expr X X -> f Text
variable exiter logger name expr' = eval exiter logger $ do
  expr <- toJS expr'
  pure (render $ CodeGen.declaration name expr)
