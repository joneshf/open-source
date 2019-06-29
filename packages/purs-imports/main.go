package main

import (
	"fmt"
	"strings"
)

func findImport(str string) (string, error) {
	tokens := strings.Split(str, " ")
	if len(tokens) > 1 {
		importToken := tokens[0]
		if importToken == "import" {
			return tokens[1], nil
		}
	}
	return str, fmt.Errorf("%#v is not a valid import", str)
}
