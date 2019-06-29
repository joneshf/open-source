package main

import (
	"strings"
)

func findImport(str string) (string, error) {
	tokens := strings.Split(str, " ")
	if len(tokens) > 2 {
		importToken := tokens[0]
		if importToken == "import" {
			return tokens[1], nil
		}
	}
	return str, nil
}
