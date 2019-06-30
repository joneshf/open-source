package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	_, err := parsePSModule(scanner)
	if err != nil {
		log.Fatalln(err)
	}
}

type psModule struct {
	module  string
	imports []string
}

func findImports(scanner *bufio.Scanner) (imports []string) {
	for scanner.Scan() {
		parsedImport, parsedImportOk := parseImport(scanner.Text())
		if parsedImportOk {
			imports = append(imports, parsedImport)
		}
	}
	sort.Strings(imports)

	return
}

func findModule(scanner *bufio.Scanner) (module string, err error) {
	var ok bool
	for scanner.Scan() {
		module, ok = parseModule(scanner.Text())
		if ok {
			break
		}
	}
	if !ok {
		err = fmt.Errorf("Could not parse module")
	}
	return
}

func graph(module psModule) string {
	var builder strings.Builder
	builder.WriteString("digraph imports {\n")
	fmt.Fprintf(&builder, "  %#v;\n", module.module)
	for _, psImport := range module.imports {
		fmt.Fprintf(&builder, "  %#v -> %#v;\n", module.module, psImport)
	}
	builder.WriteString("}")
	return builder.String()
}

func parseByPrefix(prefix, str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 && tokens[0] == prefix {
		return tokens[1], true
	}
	return "", false
}

func parseImport(str string) (string, bool) {
	return parseByPrefix("import", str)
}

func parseModule(str string) (string, bool) {
	return parseByPrefix("module", str)
}

func parsePSModule(scanner *bufio.Scanner) (result psModule, err error) {
	module, err := findModule(scanner)
	if err == nil {
		result = psModule{
			module:  module,
			imports: findImports(scanner),
		}
	}
	return
}
