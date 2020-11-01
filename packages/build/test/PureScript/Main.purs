module Main (main) where

import Prelude

import Effect as Effect
import Effect.Console as Effect.Console
import Control.Extend as Control.Extend

import Foo.Bar.Baz as Foo.Bar.Baz

main :: Effect.Effect Unit
main = Effect.Console.logShow Foo.Bar.Baz.baz
