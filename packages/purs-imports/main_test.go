/*
Copyright 2019 Hardy Jones

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package main

import (
	"reflect"
	"strings"
	"testing"
)

func TestGraph(t *testing.T) {
	input := []psModule{
		psModule{module: "Main", imports: []string{"Effect", "Prelude"}},
		psModule{module: "Test.Main", imports: []string{"Effect", "Prelude", "Test.Unit"}},
	}
	expected := `digraph imports {
  "Main";
  "Main" -> "Effect";
  "Main" -> "Prelude";
  "Test.Main";
  "Test.Main" -> "Effect";
  "Test.Main" -> "Prelude";
  "Test.Main" -> "Test.Unit";
}`
	actual := graph(input)
	if !reflect.DeepEqual(expected, actual) {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

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
			actual, ok := parseImport(test.input)
			if !ok {
				t.Errorf("Expected %#v to parse: %#v.", test.input, actual)
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
			actual, ok := parseImport(test.input)
			if ok {
				t.Errorf("Expected %#v not to parse: %#v.", test.input, actual)
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
			actual, ok := parseModule(test.input)
			if !ok {
				t.Errorf("Expected %#v to parse: %#v.", test.input, actual)
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
			actual, ok := parseModule(test.input)
			if ok {
				t.Errorf("Expected an error: %s.", actual)
			}
		})
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
	{
		input: `
module Main (main) where

import Prelude

import Effect as Effect

main :: Effect.Effect Unit
main = pure unit
`,
		expected: psModule{module: "Main", imports: []string{"Effect", "Prelude"}},
	},
}

func TestParsePSModuleDoesNotFail(t *testing.T) {
	for _, test := range parsePSModuleDoesNotFail {
		t.Run(test.input, func(t *testing.T) {
			actual, err := parsePSModule(strings.NewReader(test.input))
			if err != nil {
				t.Errorf("Expected an error: %#v.", actual)
			}
			if !reflect.DeepEqual(test.expected, actual) {
				t.Errorf("Expected: %#v. Actual: %#v.", test.expected, actual)
			}
		})
	}
}

var parsePSModuleFails = []struct {
	input string
}{
	{input: ""},
	{input: "module"},
	{input: "module' Foo = 12"},
	{input: "import Foo"},
}

func TestParsePSModuleFails(t *testing.T) {
	for _, test := range parsePSModuleFails {
		t.Run(test.input, func(t *testing.T) {
			actual, err := parsePSModule(strings.NewReader(test.input))
			if err == nil {
				t.Errorf("Expected an error: %s.", actual)
			}
		})
	}
}
