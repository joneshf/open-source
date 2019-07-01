package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

func main() {
	var modules []psModule
	args := flag.Args()
	for _, glob := range args {
		files, globErr := filepath.Glob(glob)
		if globErr != nil {
			log.Fatalln(globErr)
		}
		for range files {
		}
		module, err := parsePSModule(os.Stdin)
		if err != nil {
			log.Fatalln(err)
		}
		modules = append(modules, module)
	}
	fmt.Printf("%s\n", graph(modules))
}

type psModule struct {
	module  string
	imports []string
}

func findImports(scanner *bufio.Scanner) []string {
	var imports []string
	for scanner.Scan() {
		parsedImport, parsedImportOk := parseImport(scanner.Text())
		if parsedImportOk {
			imports = append(imports, parsedImport)
		}
	}
	sort.Strings(imports)

	return imports
}

func findModule(scanner *bufio.Scanner) (string, error) {
	for scanner.Scan() {
		module, ok := parseModule(scanner.Text())
		if ok {
			return module, nil
		}
	}
	return "", fmt.Errorf("Could not parse module")
}

func graph(modules []psModule) string {
	var builder strings.Builder
	builder.WriteString("digraph imports {\n")
	for _, module := range modules {
		fmt.Fprintf(&builder, "  %#v;\n", module.module)
		for _, psImport := range module.imports {
			fmt.Fprintf(&builder, "  %#v -> %#v;\n", module.module, psImport)
		}
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

func parsePSModule(reader io.Reader) (psModule, error) {
	scanner := bufio.NewScanner(reader)
	module, err := findModule(scanner)
	if err != nil {
		return psModule{}, err
	}
	return psModule{
		module:  module,
		imports: findImports(scanner),
	}, nil
}
