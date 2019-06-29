package main

import (
	"testing"
)

func TestFindImportDoesNotFail(t *testing.T) {
	findImportTests := []struct {
		input    string
		expected string
	}{}
	if len(findImportTests) == 0 {
	}
	input := "import Data.Array"
	expected := "Data.Array"
	t.Run(input, func(t *testing.T) {
		actual, err := findImport(input)
		if err != nil {
			t.Error("Did not expect an error", err)
		}
		if expected != actual {
			t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
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
