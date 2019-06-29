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

func parsePSModule(scanner *bufio.Scanner) (psModule, error) {
	result := psModule{}
	for scanner.Scan() {
		module, moduleOk := parseModule(scanner.Text())
		if moduleOk {
			result.module = module
			for scanner.Scan() {
				parsedImport, ok := parseImport(scanner.Text())
				if ok {
					result.imports = append(result.imports, parsedImport)
				}
			}
			sort.Strings(result.imports)
			return result, nil
		}
	}
	return psModule{}, fmt.Errorf("Could not parse module")
}

func parseByPrefix(prefix, str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 {
		prefixToken := tokens[0]
		if prefixToken == prefix {
			return tokens[1], true
		}
	}
	return "", false
}
