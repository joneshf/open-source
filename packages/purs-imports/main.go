package main

import (
	"bufio"
	"fmt"
	"strings"
)

type psModule struct {
	module  string
	imports []string
}

func parseImport(str string) (string, error) {
	return parseByPrefix("import", str)
}

func parseModule(str string) (string, error) {
	return parseByPrefix("module", str)
}

func parsePSModule(scanner *bufio.Scanner) (psModule, error) {
	return psModule{}, fmt.Errorf("Could not parse module")
}

func parseByPrefix(prefix, str string) (string, error) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 {
		prefixToken := tokens[0]
		if prefixToken == prefix {
			return tokens[1], nil
		}
	}
	return str, fmt.Errorf("%#v is not a valid %s", str, prefix)
}
