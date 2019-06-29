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

func parseImport(str string) (string, error) {
	return oldParseByPrefix("import", str)
}

func oldParseImport(str string) (string, error) {
	return oldParseByPrefix("import", str)
}

func parseModule(str string) (string, error) {
	return oldParseByPrefix("module", str)
}

func parsePSModule(scanner *bufio.Scanner) (psModule, error) {
	result := psModule{}
	for scanner.Scan() {
		module, errModule := parseModule(scanner.Text())
		if errModule == nil {
			result.module = module
			for scanner.Scan() {
				parsedImport, errImport := parseImport(scanner.Text())
				if errImport == nil {
					result.imports = append(result.imports, parsedImport)
				}
			}
			sort.Strings(result.imports)
			return result, nil
		}
	}
	return result, fmt.Errorf("Could not parse module")
}

func parseByPrefix(prefix, str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 {
		prefixToken := tokens[0]
		if prefixToken == prefix {
			return tokens[1], true
		}
	}
	return str, false
}

func oldParseByPrefix(prefix, str string) (string, error) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 {
		prefixToken := tokens[0]
		if prefixToken == prefix {
			return tokens[1], nil
		}
	}
	return str, fmt.Errorf("%#v is not a valid %s", str, prefix)
}
