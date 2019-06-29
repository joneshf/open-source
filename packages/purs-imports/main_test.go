/*
Copyright 2019 Hardy Jones

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package main

import (
	"testing"
)

func TestParseImportDoesNotFailForDataArray(t *testing.T) {
	input := "import Data.Array"
	expected := "Data.Array"
	actual, err := parseImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

func TestParseImportDoesNotFailForEffect(t *testing.T) {
	input := "import Effect"
	expected := "Effect"
	actual, err := parseImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

func TestParseImportDoesNotFailForPrelude(t *testing.T) {
	input := "import Prelude"
	expected := "Prelude"
	actual, err := parseImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

func TestParseImportDoesNotFailForPreludeWithWhitespace(t *testing.T) {
	input := "import           Prelude          "
	expected := "Prelude"
	actual, err := parseImport(input)
	if err != nil {
		t.Error("Did not expect an error", err)
	}
	if expected != actual {
		t.Errorf("Expected: %#v. Actual: %#v.", expected, actual)
	}
}

func TestParseImportFailsForTheEmptyString(t *testing.T) {
	input := ""
	actual, err := parseImport(input)
	if err == nil {
		t.Error("Expected an error", actual)
	}
}
