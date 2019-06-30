package main

import (
	"bufio"
	"fmt"
	"sort"
	"strings"
)

type psModule struct {
	module  string
	imports []string
}

func parseImport(str string) (string, bool) {
	return parseByPrefix("import", str)
}

func parseModule(str string) (string, bool) {
	return parseByPrefix("module", str)
}

func parsePSImports(scanner *bufio.Scanner, result *psModule) []string {
	for scanner.Scan() {
		parsedImport, parsedImportOk := parseImport(scanner.Text())
		if parsedImportOk {
			result.imports = append(result.imports, parsedImport)
		}
	}
	sort.Strings(result.imports)

	return result.imports
}

func parsePSModule(scanner *bufio.Scanner) (psModule, error) {
	result := psModule{}
	for scanner.Scan() {
		parsedModule, parsedModuleOk := parseModule(scanner.Text())
		if parsedModuleOk {
			result.module = parsedModule
			result.imports = parsePSImports(scanner, &result)
			return result, nil
		}
	}
	return psModule{}, fmt.Errorf("Could not parse module")
}

func parseByPrefix(prefix, str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 && tokens[0] == prefix {
		return tokens[1], true
	}
	return "", false
}
