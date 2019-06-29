package main

import (
	"fmt"
	"strings"
)

func findImport(str string) (string, error) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 {
		importToken := tokens[0]
		if importToken == "import" {
			return tokens[1], nil
		}
	}
	return str, fmt.Errorf("%#v is not a valid import", str)
}

func findModule(str string) (string, error) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 {
		moduleToken := tokens[0]
		if moduleToken == "module" {
			return tokens[1], nil
		}
	}
	return str, fmt.Errorf("%#v is not a valid module", str)
}

func findByPrefix(prefix string) func(string) (string, error) {
	return func(str string) (string, error) {
		tokens := strings.Fields(str)
		if len(tokens) > 1 {
			prefixToken := tokens[0]
			if prefixToken == prefix {
				return tokens[1], nil
			}
		}
		return str, fmt.Errorf("%#v is not a valid %s", str, prefix)
	}
}
