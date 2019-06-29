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
	{input: "import           Prelude          ", expected: "Prelude"},
}

func TestFindImportDoesNotFail(t *testing.T) {
	for _, test := range findImportDoesNotFail {
		t.Run(test.input, func(t *testing.T) {
			actual, err := findImport(test.input)
			if err != nil {
				t.Error("Did not expect an error", err)
			}
			if test.expected != actual {
				t.Errorf("Expected: %#v. Actual: %#v.", test.expected, actual)
			}
		})
	}
}

func TestFindImportFailsForTheEmptyString(t *testing.T) {
	input := ""
	actual, err := findImport(input)
	if err == nil {
		t.Error("Expected an error", actual)
	}
}
