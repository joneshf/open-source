package main

import (
	"fmt"
	"strings"
)

func findImport(str string) (string, error) {
	return findByPrefix("import", str)
}

func findModule(str string) (string, error) {
	return findByPrefix("module", str)
}

func findByPrefix(prefix, str string) (string, error) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 {
		prefixToken := tokens[0]
		if prefixToken == prefix {
			return tokens[1], nil
		}
	}
	return str, fmt.Errorf("%#v is not a valid %s", str, prefix)
}

func parseImport(str string) (string, error) {
	return parseByPrefix("import", str)
}

func parseModule(str string) (string, error) {
	return parseByPrefix("module", str)
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
