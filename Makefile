BIN ?= bin
CABAL ?= cabal
DIST ?= dist
EMPTY ?= .make
PROJECT_NAME ?= rollbar-hs
STACK ?= stack
STACK_WORK ?= .stack-work
VERBOSITY ?= warn

CABAL_FILE := $(PROJECT_NAME).cabal
GHCID := $(BIN)/ghcid
STACK_FLAGS := --verbosity $(VERBOSITY)

.DEFAULT_GOAL := build

$(BIN) $(DIST) $(EMPTY):
	mkdir -p $@

$(EMPTY)/stack-setup: | $(EMPTY)
	$(STACK) $(STACK_FLAGS) setup
	touch $@

$(GHCID): $(EMPTY)/stack-setup | $(BIN)
	$(STACK) $(STACK_FLAGS) install ghcid --local-bin-path $(BIN)

$(CABAL_FILE): package.yaml
	# `stack` has no way to run `hpack` directly.
	# We can run `hpack` indirectly with little overhead.
	$(STACK) $(STACK_FLAGS) build --dry-run

.PHONY: build
build: $(EMPTY)/stack-setup
	$(STACK) $(STACK_FLAGS) build --no-run-tests --test

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
test: build
	$(STACK) $(STACK_FLAGS) test

.PHONY: upload-hackage
upload-hackage: sdist
	@ $(CABAL) upload $(DIST)/$(PROJECT_NAME)-*.tar.gz

.PHONY: watch
watch: $(GHCID)
	$(GHCID)
