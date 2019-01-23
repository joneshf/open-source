-- |
-- Module: CodeGen
-- Description: Generate JavaScript code.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module CodeGen
  ( CodeGen.and
  , add
  , array
  , assign
  , assign'
  , bool
  , brack
  , call0
  , call1
  , call2
  , call3
  , declaration
  , eq
  , for
  , forin
  , func
  , func'
  , func2
  , ift
  , ifte
  , int
  , kind
  , lambda
  , lambda'
  , lambda''
  , lambda2
  , lambda2'
  , lt
  , CodeGen.mod
  , module'
  , mul
  , neq
  , number
  , object
  , CodeGen.or
  , preinc
  , prop
  , propId
  , property
  , returns
  , string
  , ternary
  , type'
  , var
  ) where

import "freer-simple" Control.Monad.Freer         (Eff, Member)
import "freer-simple" Control.Monad.Freer.Fresh   (Fresh)
import "scientific" Data.Scientific               (Scientific)
import "text" Data.Text                           (Text)
import "dhall" Dhall.Core                         (Var)
import "language-ecmascript" Language.ECMAScript3
    ( Expression
    , Id
    , Prop
    , Statement
    )

import qualified "base" Data.Foldable
import qualified "scientific" Data.Scientific
import qualified "text" Data.Text
import qualified "dhall" Dhall.Core
import qualified "this" Fresh
import qualified "base" GHC.Exts
import qualified "language-ecmascript" Language.ECMAScript3
import qualified "language-ecmascript" Language.ECMAScript3.Syntax
import qualified "language-ecmascript" Language.ECMAScript3.Syntax.CodeGen

-- |
-- A lawless class to make some functions in here a bit easier to use.
class ToExpression a where
  toExpression :: a -> Expression ()

instance ToExpression (Expression ()) where
  toExpression = id

instance ToExpression (Id ()) where
  toExpression = Language.ECMAScript3.Syntax.CodeGen.var

-- |
-- @'add' x y ~ x + y@
add :: Expression () -> Expression () -> Eff e (Expression ())
add x = pure . Language.ECMAScript3.Syntax.CodeGen.add x

-- |
-- @'and' x y ~ x && y@
and :: Expression () -> Expression () -> Eff e (Expression ())
and x = pure . Language.ECMAScript3.Syntax.CodeGen.land x

-- |
-- @'array' xs ~ xs@
array :: (Foldable f) => f (Expression ()) -> Eff e (Expression ())
array = pure . Language.ECMAScript3.Syntax.CodeGen.array . Data.Foldable.toList

-- |
-- @'assign' x y ~ x = y;@
assign :: Text -> Expression () -> Statement ()
assign name expression =
  Language.ECMAScript3.Syntax.CodeGen.expr
    ( Language.ECMAScript3.Syntax.CodeGen.assign
      (Language.ECMAScript3.Syntax.CodeGen.lvar $ Data.Text.unpack name)
      expression
    )

-- |
-- @'assign'' x y ~ x = y;@
assign' :: Expression () -> Expression () -> Expression () -> Statement ()
assign' var' prop' expression =
  Language.ECMAScript3.Syntax.CodeGen.expr
    ( Language.ECMAScript3.Syntax.CodeGen.assign
      (Language.ECMAScript3.Syntax.CodeGen.lbrack var' prop')
      expression
    )

-- |
-- @
-- 'bool' True ~ true
-- 'bool' False ~ false
-- @
bool :: Bool -> Eff e (Expression ())
bool = pure . Language.ECMAScript3.Syntax.CodeGen.bool

-- |
-- @'brack' x y ~ x[y]@
brack :: Expression () -> Expression () -> Eff e (Expression ())
brack x = pure . Language.ECMAScript3.Syntax.CodeGen.brack x

-- |
-- @'call0' f () ~ f()@
call0 ::
  (ToExpression a) =>
  Eff e a ->
  () ->
  Eff e (Expression ())
call0 f' _ = do
  f <- f'
  pure (Language.ECMAScript3.Syntax.CodeGen.call (toExpression f) [])

-- |
-- @'call1' f x ~ f(x)@
call1 ::
  (ToExpression a, ToExpression b) =>
  Eff e a ->
  b ->
  Eff e (Expression ())
call1 f' x = do
  f <- f'
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.call
      (toExpression f)
      [toExpression x]
    )

-- |
-- @'call2' f (x, y) ~ f(x, y)@
call2 ::
  (ToExpression a, ToExpression b, ToExpression c) =>
  Eff e a ->
  (b, c) ->
  Eff e (Expression ())
call2 f' (x, y) = do
  f <- f'
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.call
      (toExpression f)
      [toExpression x, toExpression y]
    )

-- |
-- @'call3' f (x, y, z) ~ f(x, y, z)@
call3 ::
  (ToExpression a, ToExpression b, ToExpression c, ToExpression d) =>
  Eff e a ->
  (b, c, d) ->
  Eff e (Expression ())
call3 f' (x, y, z) = do
  f <- f'
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.call
      (toExpression f)
      [toExpression x, toExpression y, toExpression z]
    )

-- |
-- @'declaration' x y ~ var x = y;@
declaration :: Text -> Expression () -> Statement ()
declaration name expression =
  Language.ECMAScript3.Syntax.CodeGen.vardecls
   [ Language.ECMAScript3.Syntax.CodeGen.varinit
     (GHC.Exts.fromString $ Data.Text.unpack name)
     expression
   ]

-- |
-- @'dot' x y ~ x.y@
dot :: (ToExpression a) => a -> Id () -> Eff e (Expression ())
dot x = pure . Language.ECMAScript3.Syntax.CodeGen.dot (toExpression x)

-- |
-- @'eq' x y ~ x === y@
eq :: Expression () -> Expression () -> Eff e (Expression ())
eq x = pure . Language.ECMAScript3.Syntax.CodeGen.steq x

-- |
-- @
-- 'for' ((x, y), test, increment) [statement1, statement2, ..., statementN] ~
--   for (var x = y; test; increment) {
--     statement1;
--     statement2;
--     ...
--     statementN;
--   }
-- @
for ::
  ( (Text, Eff e (Expression ()))
  , Eff e (Expression ())
  , Eff e (Expression ())
  ) ->
  [Statement ()] ->
  Eff e (Statement ())
for ((name, expression'), test', increment') body = do
  expression <- expression'
  test <- test'
  increment <- increment'
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.for
      ( Language.ECMAScript3.Syntax.VarInit
        [ Language.ECMAScript3.Syntax.CodeGen.varinit
          (GHC.Exts.fromString $ Data.Text.unpack name)
          expression
        ]
      )
      (Just test)
      (Just increment)
      (Language.ECMAScript3.Syntax.CodeGen.block body)
    )

-- |
-- @
-- 'forin' (x, y) [statement1, statement2, ..., statementN] ~
--   for (var x in y) {
--     statement1;
--     statement2;
--     ...
--     statementN;
--   }
-- @
forin ::
  ( Text
  , Eff e (Expression ())
  ) ->
  [Statement ()] ->
  Eff e (Statement ())
forin (name, expression') body = do
  expression <- expression'
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.forin
      ( Language.ECMAScript3.Syntax.ForInVar
        $ GHC.Exts.fromString
        $ Data.Text.unpack name
      )
      expression
      (Language.ECMAScript3.Syntax.CodeGen.block body)
    )

-- |
-- @
-- 'func' name f ~
--   function name(x) {
--     return body;
--   }
-- @
--
-- where @x@ is a fresh 'Id', and @body@ is the result of @f x@.
func ::
  (Member Fresh e) =>
  Text ->
  (Id () -> Eff e (Expression ())) ->
  Eff e (Expression ())
func name f = do
  arg <- Fresh.ident
  body <- f arg
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.func
      (GHC.Exts.fromString $ Data.Text.unpack name)
      [arg]
      [Language.ECMAScript3.Syntax.CodeGen.returns body]
    )

-- |
-- @
-- 'func'' name f ~
--   function name(x) {
--     return body;
--   }
-- @
--
-- where @x@ is a fresh 'Var', and @body@ is the result of @f x@.
func' ::
  (Member Fresh e) =>
  Text ->
  (Var -> Eff e (Expression ())) ->
  Eff e (Expression ())
func' name f = do
  v@(Dhall.Core.V x _) <- Fresh.v
  body <- f v
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.func
      (GHC.Exts.fromString $ Data.Text.unpack name)
      [GHC.Exts.fromString $ Data.Text.unpack x]
      [Language.ECMAScript3.Syntax.CodeGen.returns body]
    )

-- |
-- @
-- 'lambda' name f ~
--   function name(x, y) {
--     statement1;
--     statement2;
--     ...;
--     statementN;
--   }
-- @
--
-- where @x@ and @y@ are a fresh 'Id's,
-- and @statement1@ through @statementN@ are the results of @f (x, y)@.
func2 ::
  (Member Fresh e) =>
  Text ->
  ((Id (), Id ()) -> Eff e [Statement ()]) ->
  Eff e (Expression ())
func2 name f = do
  arg1 <- Fresh.ident
  arg2 <- Fresh.ident
  body <- f (arg1, arg2)
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.func
      (GHC.Exts.fromString $ Data.Text.unpack name)
      [arg1, arg2]
      body
    )

-- |
-- @
-- 'ift' x [then1, then2, ..., thenN] ~
--   if (x) {
--     then1
--     then2
--     ...
--     thenN
--   }
ift :: Expression () -> [Statement ()] -> Statement ()
ift test thens =
  Language.ECMAScript3.Syntax.CodeGen.ift
    test
    (Language.ECMAScript3.Syntax.CodeGen.block thens)

-- |
-- @
-- 'ifte' x [then1, then2, ..., thenM] [else1, else2, ..., elseN] ~
--   if (x) {
--     then1
--     then2
--     ...
--     thenM
--   } else {
--     else1
--     else2
--     ...
--     elseN
--   }
ifte :: Expression () -> [Statement ()] -> [Statement ()] -> Statement ()
ifte test thens elses =
  Language.ECMAScript3.Syntax.CodeGen.ifte
    test
    (Language.ECMAScript3.Syntax.CodeGen.block thens)
    (Language.ECMAScript3.Syntax.CodeGen.block elses)

-- |
-- @'int' x ~ x@
int :: (Integral a) => a -> Eff e (Expression ())
int = pure . Language.ECMAScript3.Syntax.CodeGen.int . fromIntegral

-- |
-- @'kind' ~ null@
kind :: Eff e (Expression ())
kind = pure Language.ECMAScript3.Syntax.CodeGen.null_

-- |
-- @
-- 'lambda' f ~
--   function(x) {
--     return body;
--   }
-- @
--
-- where @x@ is a fresh 'Id', and @body@ is the result of @f x@.
lambda ::
  (Member Fresh e) =>
  (Id () -> Eff e (Expression ())) ->
  Eff e (Expression ())
lambda f = do
  arg <- Fresh.ident
  lambda' arg (f arg)

-- |
-- @
-- 'lambda'' x body ~
--   function(x) {
--     return body;
--   }
-- @
--
-- N.B. This differs from 'lambda' in that the given 'Id' is used directly.
-- Since this value might shadow others,
-- this function should only be used with a 'Fresh' 'Id' or a user-supplied 'Id'.
lambda' :: Id () -> Eff e (Expression ()) -> Eff e (Expression ())
lambda' arg body' = do
  body <- body'
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.lambda
      [arg]
      [Language.ECMAScript3.Syntax.CodeGen.returns body]
    )

-- |
-- @
-- 'lambda' f ~
--   function(x) {
--     statement1;
--     statement2;
--     ...;
--     statementN;
--   }
-- @
--
-- where @x@ is a fresh 'Id',
-- and @statement1@ through @statementN@ are the results of @f x@.
lambda'' ::
  (Member Fresh e) =>
  (Id () -> Eff e [Statement ()]) ->
  Eff e (Expression ())
lambda'' f = do
  arg <- Fresh.ident
  body <- f arg
  pure (Language.ECMAScript3.Syntax.CodeGen.lambda [arg] body)

-- |
-- @
-- 'lambda2' f ~
--   function(x, y) {
--     return body;
--   }
-- @
--
-- where @x@ and @y@ are fresh 'Id's, and @body@ is the result of @f (x, y)@.
lambda2 ::
  (Member Fresh e) =>
  ((Id (), Id ()) -> Eff e (Expression ())) ->
  Eff e (Expression ())
lambda2 f = do
  arg1 <- Fresh.ident
  arg2 <- Fresh.ident
  lambda2' (arg1, arg2) (f (arg1, arg2))

-- |
-- @
-- 'lambda2'' (x, y) body ~
--   function(x, y) {
--     return body;
--   }
-- @
--
-- N.B. This differs from 'lambda2' in that the given 'Id's are used directly.
-- Since these values might shadow others,
-- this function should only be used with 'Fresh' 'Id's or user-supplied 'Id's.
lambda2' :: (Id (), Id ()) -> Eff e (Expression ()) -> Eff e (Expression ())
lambda2' (arg1, arg2) body' = do
  body <- body'
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.lambda
      [arg1, arg2]
      [Language.ECMAScript3.Syntax.CodeGen.returns body]
    )

-- |
-- @'lt' x y ~ x < y@
lt :: Expression () -> Expression () -> Eff e (Expression ())
lt x = pure . Language.ECMAScript3.Syntax.CodeGen.lt x

-- |
-- @'mod' x y ~ x % y@
mod :: Expression () -> Expression () -> Eff e (Expression ())
mod x = pure . Language.ECMAScript3.Syntax.CodeGen.mod x

-- |
-- @'module'' x ~ module.exports = x;@
module' :: Expression () -> Eff e (Expression ())
module' x = do
  m <- var "module"
  pure
    ( Language.ECMAScript3.Syntax.CodeGen.assign
      (Language.ECMAScript3.Syntax.CodeGen.ldot m "exports")
      x
    )

-- |
-- @'mul' x y ~ x * y@
mul :: Expression () -> Expression () -> Eff e (Expression ())
mul x = pure . Language.ECMAScript3.Syntax.CodeGen.mul x

-- |
-- @'neq' x y ~ x !== y@
neq :: Expression () -> Expression () -> Eff e (Expression ())
neq x = pure . Language.ECMAScript3.Syntax.CodeGen.stneq x

-- |
-- @'number' x ~ x@
number :: Scientific -> Eff e (Expression ())
number =
  pure . Language.ECMAScript3.Syntax.CodeGen.number . Data.Scientific.toRealFloat

-- |
-- @'literal' [(key1, val1), (key2, val2), ...] ~ {key1: val1, key2: val2, ...}@
--
-- N.B. Will escape keys according to the rules of 'prop'.
object :: (Foldable f) => f (Prop (), Expression ()) -> Eff e (Expression ())
object =
  pure . Language.ECMAScript3.Syntax.CodeGen.object . Data.Foldable.toList

-- |
-- @'or' x y ~ x || y@
or :: Expression () -> Expression () -> Eff e (Expression ())
or x = pure . Language.ECMAScript3.Syntax.CodeGen.lor x

-- |
-- @'preinc' x ~ ++x@
preinc :: Text -> Eff e (Expression ())
preinc =
  pure
    . Language.ECMAScript3.Syntax.CodeGen.preinc
    . Language.ECMAScript3.Syntax.CodeGen.lvar
    . Data.Text.unpack

-- |
-- @'prop' x ~ x@
--
-- if @x@ is a valid JavaScript identifier.
--
-- @'prop' x ~ "x"@
--
-- if @x@ is not a valid JavaScript identifier.
prop :: Text -> Prop ()
prop key'
  | Language.ECMAScript3.isValidIdentifierName key =
    Language.ECMAScript3.Syntax.CodeGen.propId $ GHC.Exts.fromString key
  | otherwise = GHC.Exts.fromString key
  where
  key = Data.Text.unpack key'

-- |
-- @'propId' x ~ x@
propId :: Id () -> Prop ()
propId = Language.ECMAScript3.Syntax.CodeGen.propId

-- |
-- @'property' x y ~ x.y@
--
-- if @y@ is a valid JavaScript identifier.
--
-- @'property' x y ~ x[y]@
--
-- if @y@ is not a valid JavaScript identifier.
property :: (ToExpression a) => a -> Text -> Eff e (Expression ())
property expression key'
  | Language.ECMAScript3.isValidIdentifierName key =
    toExpression expression `dot` GHC.Exts.fromString key
  | otherwise = toExpression expression `brack` GHC.Exts.fromString key
  where
  key = Data.Text.unpack key'

-- |
-- @'returns' x ~ return x;@
returns :: Expression () -> Statement ()
returns = Language.ECMAScript3.Syntax.CodeGen.returns

-- |
-- @'string' x ~ x@
string :: Text -> Eff e (Expression ())
string = pure . Language.ECMAScript3.Syntax.CodeGen.string . Data.Text.unpack

-- |
-- @'ternary' x y z ~ x ? y : z@
ternary ::
  Expression () ->
  Expression () ->
  Expression () ->
  Eff e (Expression ())
ternary x y = pure . Language.ECMAScript3.Syntax.CodeGen.cond x y

-- |
-- @'type'' ~ null@
type' :: Eff e (Expression ())
type' = pure Language.ECMAScript3.Syntax.CodeGen.null_

-- |
-- @'var' x ~ x@
var :: Id () -> Eff e (Expression ())
var = pure . Language.ECMAScript3.Syntax.CodeGen.var
