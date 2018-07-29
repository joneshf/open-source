BIN ?= bin
CABAL ?= cabal
DIST ?= dist
EMPTY ?= .make
PROJECT_NAME ?= rollbar-hs
STACK ?= stack
STACK_WORK ?= .stack-work
VERBOSITY ?= warn

CABAL_FILE := $(PROJECT_NAME).cabal
DOC_TEST := $(DIST)/build/doc-test/doc-test
GHCID := $(BIN)/ghcid
STACK_FLAGS := --verbosity $(VERBOSITY)

.DEFAULT_GOAL := $(EMPTY)/build

$(BIN) $(DIST) $(EMPTY):
	mkdir -p $@

$(CABAL_FILE): package.yaml
	# `stack` has no way to run `hpack` directly.
	# We can run `hpack` indirectly with little overhead.
	$(STACK) $(STACK_FLAGS) build --dry-run

$(DOC_TEST): $(EMPTY)/build golden/**/*.json
	$@

$(EMPTY)/build: $(EMPTY)/stack-setup README.md Setup.hs package.yaml stack.yaml src/**/*.hs test/**/*.hs | $(DIST)
	$(STACK) $(STACK_FLAGS) build --no-run-tests --test
	cp -R $$($(STACK) $(STACK_FLAGS) path --dist-dir)/build $(DIST)/build
	touch $@

$(EMPTY)/doc-test: $(EMPTY)/build
	touch $@

$(EMPTY)/stack-setup: | $(EMPTY)
	$(STACK) $(STACK_FLAGS) setup
	touch $@

$(GHCID): $(EMPTY)/stack-setup | $(BIN)
	$(STACK) $(STACK_FLAGS) install ghcid --local-bin-path $(BIN)

.PHONY: cabal-check
cabal-check: $(CABAL_FILE)
	$(CABAL) check

.PHONY: clean
clean:
	rm -f $(CABAL_FILE)
	rm -fr $(BIN)
	rm -fr $(DIST)
	rm -fr $(EMPTY)
	rm -fr $(STACK_WORK)

.PHONY: sdist
sdist: cabal-check | $(DIST)
	$(CABAL) sdist

.PHONY: test
test: $(DOC_TEST)

.PHONY: upload-hackage
upload-hackage: sdist
	@ $(CABAL) upload $(DIST)/$(PROJECT_NAME)-*.tar.gz

.PHONY: watch
watch: $(GHCID)
	$(GHCID)
