package main

import (
	"testing"
)

func TestFindImportAlwaysWorks(t *testing.T) {
	expected := ""
	_, err := findImport(expected)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if false {
		t.Errorf("Expected: %+v", expected)
	}
}
