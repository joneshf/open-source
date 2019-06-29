package main

import (
	"testing"
)

func TestFindImportDoesNotFailForValidInput(t *testing.T) {
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
