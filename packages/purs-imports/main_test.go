package main

import (
	"bufio"
	"reflect"
	"strings"
	"testing"
)

var parseImportDoesNotFail = []struct {
	input    string
	expected string
}{
	{input: "import Data.Array", expected: "Data.Array"},
	{input: "import Effect", expected: "Effect"},
	{input: "import Prelude", expected: "Prelude"},
	{input: "    import           Prelude          ", expected: "Prelude"},
}

func TestParseImportDoesNotFail(t *testing.T) {
	for _, test := range parseImportDoesNotFail {
		t.Run(test.input, func(t *testing.T) {
			actual, err := parseImport(test.input)
			if err != nil {
				t.Errorf("Did not expect an error: %s.", err)
			}
			if test.expected != actual {
				t.Errorf("Expected: %#v. Actual: %#v.", test.expected, actual)
			}
		})
	}
}

var parseImportFails = []struct {
	input string
}{
	{input: ""},
	{input: "import"},
	{input: "import' Foo = 12"},
	{input: "module Foo"},
}

func TestParseImportFails(t *testing.T) {
	for _, test := range parseImportFails {
		t.Run(test.input, func(t *testing.T) {
			actual, err := parseImport(test.input)
			if err == nil {
				t.Errorf("Expected an error: %s.", actual)
			}
		})
	}
}

var parseModuleDoesNotFail = []struct {
	input    string
	expected string
}{
	{input: "module Data.Array where", expected: "Data.Array"},
	{input: "module Effect where", expected: "Effect"},
	{input: "module Prelude where", expected: "Prelude"},
	{input: "    module           Prelude           where", expected: "Prelude"},
}

func TestParseModuleDoesNotFail(t *testing.T) {
	for _, test := range parseModuleDoesNotFail {
		t.Run(test.input, func(t *testing.T) {
			actual, err := parseModule(test.input)
			if err != nil {
				t.Errorf("Did not expect an error: %s.", err)
			}
			if test.expected != actual {
				t.Errorf("Expected: %#v. Actual: %#v.", test.expected, actual)
			}
		})
	}
}

var parseModuleFails = []struct {
	input string
}{
	{input: ""},
	{input: "module"},
	{input: "module' Foo = 12"},
	{input: "import Foo"},
}

func TestParseModuleFails(t *testing.T) {
	for _, test := range parseModuleFails {
		t.Run(test.input, func(t *testing.T) {
			actual, err := parseModule(test.input)
			if err == nil {
				t.Errorf("Expected an error: %s.", actual)
			}
		})
	}
}

func TestParsePSModuleFailsForTheEmptyString(t *testing.T) {
	input := ""
	scanner := bufio.NewScanner(strings.NewReader(input))
	actual, err := parsePSModule(scanner)
	if err == nil {
		t.Errorf("Expected an error: %#v.", actual)
	}
}

var parsePSModuleDoesNotFail = []struct {
	input    string
	expected psModule
}{
	{input: "module X where", expected: psModule{module: "X"}},
	{
		input: `
module X where

import Y
import Z
`,
		expected: psModule{module: "X", imports: []string{"Y", "Z"}},
	},
	// 	{
	// 		input: `
	// module Main (main) where

	// import Prelude

	// import Effect as Effect

	// main :: Effect.Effect Unit
	// main = pure unit
	// `,
	// 		expected: psModule{module: "Main", imports: []string{"Prelude", "Effect"}},
	// 	},
}

func TestParsePSModuleDoesNotFail(t *testing.T) {
	for _, test := range parsePSModuleDoesNotFail {
		t.Run(test.input, func(t *testing.T) {
			scanner := bufio.NewScanner(strings.NewReader(test.input))
			actual, err := parsePSModule(scanner)
			if err != nil {
				t.Errorf("Expected an error: %#v.", actual)
			}
			if !reflect.DeepEqual(test.expected, actual) {
				t.Errorf("Expected: %#v. Actual: %#v.", test.expected, actual)
			}
		})
	}
}
