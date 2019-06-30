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

func parsePSImports(scanner *bufio.Scanner) (imports []string) {
	for scanner.Scan() {
		parsedImport, parsedImportOk := parseImport(scanner.Text())
		if parsedImportOk {
			imports = append(imports, parsedImport)
		}
	}
	sort.Strings(imports)

	return
}

func parsePSModule(scanner *bufio.Scanner) (result psModule, err error) {
	var module string
	var ok bool
	for scanner.Scan() {
		module, ok = parseModule(scanner.Text())
		if ok {
			break
		}
	}
	if ok {
		result = psModule{
			module:  module,
			imports: parsePSImports(scanner),
		}
	}
	if !ok {
		err = fmt.Errorf("Could not parse module")
	}
	return
}

func parseByPrefix(prefix, str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 && tokens[0] == prefix {
		return tokens[1], true
	}
	return "", false
}
