package main

import (
	"testing"
)

func TestFindImportDoesNotFailForValidInput(t *testing.T) {
	input := "import Prelude"
	_, err := findImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if false {
		t.Errorf("Expected: %#v. Actual: %#v.", "", "")
	}
}
