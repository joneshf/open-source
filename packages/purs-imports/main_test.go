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
	if false {
		t.Errorf("Expected: %+v, Actual: %+v", expected, actual)
	}
}
