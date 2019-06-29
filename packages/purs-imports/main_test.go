package main

import (
	"testing"
)

var findImportDoesNotFail = []struct {
	input    string
	expected string
}{
	{input: "import Data.Array", expected: "Data.Array"},
	{input: "import Effect", expected: "Effect"},
	{input: "import Prelude", expected: "Prelude"},
	{input: "    import           Prelude          ", expected: "Prelude"},
}

func TestFindImportDoesNotFail(t *testing.T) {
	for _, test := range findImportDoesNotFail {
		t.Run(test.input, func(t *testing.T) {
			actual, err := findImport(test.input)
			if err != nil {
				t.Errorf("Did not expect an error: %s.", err)
			}
			if test.expected != actual {
				t.Errorf("Expected: %#v. Actual: %#v.", test.expected, actual)
			}
		})
	}
}

var findImportFails = []struct {
	input string
}{
	{input: ""},
	{input: "import"},
	{input: "import' Foo = 12"},
	{input: "module Foo"},
}

func TestFindImportFailsForTheEmptyString(t *testing.T) {
	for _, test := range findImportFails {
		t.Run(test.input, func(t *testing.T) {
			actual, err := findImport(test.input)
			if err == nil {
				t.Errorf("Expected an error: %s.", actual)
			}
		})
	}
}

var findModuleFails = []struct {
	input string
}{
	{input: ""},
	{input: "module"},
	{input: "module' Foo = 12"},
	{input: "import Foo"},
}

func TestFindModuleFailsForTheEmptyString(t *testing.T) {
	for _, test := range findModuleFails {
		t.Run(test.input, func(t *testing.T) {
			actual, err := findModule(test.input)
			if err == nil {
				t.Errorf("Expected an error: %s.", actual)
			}
		})
	}
}
