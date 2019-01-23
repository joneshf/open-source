-- |
-- Module: Dhall.JavaScript
-- Description: Main entry point to the program.
--
-- Copyright: (c) Hardy Jones, 2018
-- License: BSD3
-- Maintainer: jones3.hardy@gmail.com
-- Stability: experimental
module Dhall.JavaScript
  ( main
  ) where

import "base" Control.Applicative                 ((<|>))
import "text" Data.Text                           (Text)
import "this" Log                                 (Logger)
import "optparse-applicative" Options.Applicative (Parser, ParserInfo)

import qualified "text" Data.Text
import qualified "text" Data.Text.IO
import qualified "dhall" Dhall
import qualified "dhall" Dhall.Core
import qualified "this" Exit
import qualified "this" Log
import qualified "optparse-applicative" Options.Applicative
import qualified "this" Render

-- |
-- Runs the CLI program to convert a Dhall expression to a JavaScript.
--
-- For help with the program, pass the @--help@ flag.
main :: IO ()
main = do
  Options explain'' input' output' verbosity' <-
    Options.Applicative.execParser info
  let explain' = case explain'' of
        DoExplain -> Dhall.detailed
        DoNotExplain -> id
      logger :: Logger IO
      logger = case verbosity' of
        Debug -> Log.io Log.Debug
        Error -> Log.io Log.Error
  contents <- case input' of
    File path -> Data.Text.IO.readFile (Data.Text.unpack path)
    Stdin     -> Data.Text.IO.getContents
  dhall <- fmap Dhall.Core.denote (explain' $ Dhall.inputExpr contents)
  js <- case output' of
    Expression   -> Render.expression Exit.io logger dhall
    Module       -> Render.module' Exit.io logger dhall
    Variable var -> Render.variable Exit.io logger var dhall
  Data.Text.IO.putStrLn js

info :: ParserInfo Options
info =
  Options.Applicative.info
    ( Options.Applicative.helper
    <*> parser
    )
    ( Options.Applicative.fullDesc
    <> Options.Applicative.progDesc "Compile Dhall to JavaScript"
    )

data Options
  = Options Explain Input Output Verbosity

data Explain
  = DoExplain
  | DoNotExplain

data Input
  = File Text
  | Stdin

data Output
  = Expression
  | Module
  | Variable Text

data Verbosity
  = Debug
  | Error

parser :: Parser Options
parser = Options <$> explain <*> input <*> output <*> verbosity

explain :: Parser Explain
explain =
  Options.Applicative.flag
    DoNotExplain
    DoExplain
    ( Options.Applicative.long "explain"
    <> Options.Applicative.help "Explain error messages in more detail"
    )


input :: Parser Input
input = file <|> stdin
  where
  file =
    File
      <$> Options.Applicative.strOption
        ( Options.Applicative.long "file"
        <> Options.Applicative.metavar "FILE"
        <> Options.Applicative.help "Dhall file to read from"
        )
  stdin = pure Stdin

output :: Parser Output
output = module' <|> variable <|> pure Expression
  where
  module' =
    Options.Applicative.flag'
      Module
      ( Options.Applicative.long "module"
      <> Options.Applicative.help "Declare a JavaScript module and export the Dhall expression"
      )
  variable =
    Variable
      <$> Options.Applicative.strOption
        ( Options.Applicative.long "var"
        <> Options.Applicative.metavar "NAME"
        <> Options.Applicative.help "Declare a JavaScript variable with this name and initialize with the value of the Dhall expression"
        )

verbosity :: Parser Verbosity
verbosity =
  Options.Applicative.flag
    Error
    Debug
    ( Options.Applicative.long "verbose"
    <> Options.Applicative.help "Print debug information"
    )
