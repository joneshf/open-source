/*
Copyright 2019 Hardy Jones

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

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
	var verbose bool
	flag.BoolVar(&verbose, "verbose", false, "Output debugging information")
	flag.Parse()
	args := flag.Args()
	debug(verbose, fmt.Sprintf("args: %#v\n", args))
	for _, glob := range args {
		debug(verbose, fmt.Sprintf("glob: %#v\n", glob))
		filenames, globErr := filepath.Glob(glob)
		if globErr != nil {
			log.Fatalln(globErr)
		}
		debug(verbose, fmt.Sprintf("filenames: %#v\n", filenames))
		for _, filename := range filenames {
			file, openErr := os.Open(filename)
			if openErr != nil {
				log.Fatalln(openErr)
			}

			module, parseErr := parsePSModule(file)
			if parseErr != nil {
				log.Fatalln(parseErr)
			}
			debug(verbose, fmt.Sprintf("module: %#v\n", module))
			modules = append(modules, module)
		}
	}
	debug(verbose, fmt.Sprintf("modules: %#v\n", modules))
	fmt.Printf("%s\n", graph(modules))
}

type psModule struct {
	module  string
	imports []string
}

func debug(verbose bool, message string) {
	if verbose {
		log.Print(message)
	}
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

func findModule(scanner *bufio.Scanner) (string, bool) {
	var module string
	var ok bool
	for scanner.Scan() {
		module, ok = parseModule(scanner.Text())
		if ok {
			break
		}
	}
	return module, ok
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

func parseImport(str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 && tokens[0] == "import" {
		return tokens[1], true
	}
	return "", false
}

func parseModule(str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 && tokens[0] == "module" {
		return tokens[1], true
	}
	return "", false
}

func parsePSModule(reader io.Reader) (psModule, error) {
	scanner := bufio.NewScanner(reader)
	parsedModule, parsedModuleOk := findModule(scanner)
	if parsedModuleOk {
		return psModule{
			module:  parsedModule,
			imports: findImports(scanner),
		}, nil
	}
	return psModule{}, fmt.Errorf("Could not parse module")
}
