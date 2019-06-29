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

var parseImportDoesNotFail = []struct {
	input    string
	expected string
}{
	{input: "import Data.Array", expected: "Data.Array"},
	{input: "import Effect", expected: "Effect"},
	{input: "import Prelude", expected: "Prelude"},
	{input: "    import           Prelude          ", expected: "Prelude"},
}

func TestParseImportDoesNotFail(t *testing.T) {
	for _, test := range parseImportDoesNotFail {
		t.Run(test.input, func(t *testing.T) {
			actual, err := parseImport(test.input)
			if err != nil {
				t.Errorf("Did not expect an error: %s.", err)
			}
			if test.expected != actual {
				t.Errorf("Expected: %#v. Actual: %#v.", test.expected, actual)
			}
		})
	}
}

var parseImportFails = []struct {
	input string
}{
	{input: ""},
	{input: "import"},
	{input: "import' Foo = 12"},
	{input: "module Foo"},
}

func TestParseImportFails(t *testing.T) {
	for _, test := range parseImportFails {
		t.Run(test.input, func(t *testing.T) {
			actual, err := parseImport(test.input)
			if err == nil {
				t.Errorf("Expected an error: %s.", actual)
			}
		})
	}
}
