CABAL ?= cabal
CABAL_FLAGS ?=
CABAL_BUILD_FLAGS ?=
DIST ?= dist
GHCID ?= ghcid
GHCID_FLAGS ?= --ghc-options=-fno-code
HPACK ?= hpack
NIX_SHELL ?= nix-shell
NIX_SHELL_FLAGS ?=
PROJECT_NAME := rollbar-hs

CABAL_FILE := $(PROJECT_NAME).cabal
CONFIGURE := $(DIST)/setup-config
DOC_TEST := $(DIST)/build/doc-test/doc-test

.DEFAULT_GOAL := build

$(CABAL_FILE): package.yaml
	$(HPACK)

$(CONFIGURE): $(CABAL_FILE)
	$(CABAL) $(CABAL_FLAGS) configure --enable-tests

.PHONY: build
build $(DOC_TEST): $(CONFIGURE) default.nix
	$(CABAL) $(CABAL_FLAGS) build $(CABAL_BUILD_FLAGS)

.PHONY: check
check: $(CABAL_FILE)
	$(CABAL) $(CABAL_FLAGS) check

.PHONY: clean
clean:
	rm -f $(CABAL_FILE)
	rm -fr $(DIST)

default.nix: $(CABAL_FILE)
	cabal2nix . > $@

.PHONY: sdist
sdist: check
	$(CABAL) $(CABAL_FLAGS) sdist

.PHONY: shell
shell:
	$(NIX_SHELL) --pure $(NIX_SHELL_FLAGS)

.PHONY: test
test: test-doc-test

.PHONY: test-doc-test
test-doc-test: $(DOC_TEST)
	$<

.PHONY: upload-hackage
upload-hackage: sdist
	@ $(CABAL) $(CABAL_FLAGS) upload $(DIST)/$(PROJECT_NAME)-*.tar.gz

.PHONY: watch
watch: $(CONFIGURE)
	$(GHCID) --command "cabal repl lib:$(PROJECT_NAME) $(GHCID_FLAGS)"
