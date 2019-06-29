package main

import (
	"testing"
)

func TestFindImportAlwaysWorks(t *testing.T) {
	expected := ""
	actual, err := findImport(expected)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %s, Actual: %s", expected, actual)
	}
}
