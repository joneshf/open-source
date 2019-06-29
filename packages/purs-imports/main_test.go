package main

import (
	"testing"
)

func TestFindImportAlwaysWorks(t *testing.T) {
	_, err := findImport("")
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if true {
	}
}
