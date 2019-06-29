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
	"bufio"
	"sort"
	"strings"
)

func findImports(scanner *bufio.Scanner) []string {
	var imports []string
	for scanner.Scan() {
		parsedImport, parsedImportOk := parseImport(scanner.Text())
		if parsedImportOk {
			imports = append(imports, parsedImport)
		}
	}
	sort.Strings(imports)

	return imports
}

func parseImport(str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 && tokens[0] == "import" {
		return tokens[1], true
	}
	return "", false
}
