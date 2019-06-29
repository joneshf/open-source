package main

import (
	"testing"
)

func TestFindImportDoesNotFail(t *testing.T) {
	findImportTests := []struct {
		input    string
		expected string
	}{
		{
			input:    "import Data.Array",
			expected: "Data.Array",
		},
	}
	for _, test := range findImportTests {
		if test.input == "" {
		}
	}
	test := struct {
		input    string
		expected string
	}{
		input:    "import Data.Array",
		expected: "Data.Array",
	}
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

func TestFindImportDoesNotFailForDataArray(t *testing.T) {
	input := "import Data.Array"
	expected := "Data.Array"
	actual, err := findImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

func TestFindImportDoesNotFailForEffect(t *testing.T) {
	input := "import Effect"
	expected := "Effect"
	actual, err := findImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

func TestFindImportDoesNotFailForPrelude(t *testing.T) {
	input := "import Prelude"
	expected := "Prelude"
	actual, err := findImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

func TestFindImportDoesNotFailForPreludeWithWhitespace(t *testing.T) {
	input := "import           Prelude          "
	expected := "Prelude"
	actual, err := findImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

func TestFindImportFailsForTheEmptyString(t *testing.T) {
	input := ""
	actual, err := findImport(input)
	if err == nil {
		t.Error("Expected an error", actual)
	}
}
