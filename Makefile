PROJECT_NAME := rollbar-hs

BIN := bin
CABAL := cabal
CABAL_FILE := $(PROJECT_NAME).cabal
DIST := dist
EMPTY := .make
GHCID := $(BIN)/ghcid
STACK := stack
STACK_WORK := .stack-work

.DEFAULT_GOAL := build

$(BIN):
	mkdir -p $@

$(DIST):
	mkdir -p $@

$(EMPTY):
	mkdir -p $@

$(EMPTY)/stack-setup: | $(EMPTY)
	$(STACK) setup
	touch $@

$(GHCID): $(EMPTY)/stack-setup | $(BIN)
	$(STACK) install ghcid --local-bin-path $(BIN)

$(CABAL_FILE):
	# `stack` has no way to run `hpack` directly.
	# We can run `hpack` indirectly with little overhead.
	$(STACK) build --dry-run

.PHONY: build
build: $(EMPTY)/stack-setup
	$(STACK) build --no-run-tests --test

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
	$(STACK) test

.PHONY: upload-hackage
upload-hackage: sdist
	@ $(CABAL) upload $(DIST)/$(PROJECT_NAME)-*.tar.gz

.PHONY: watch
watch: $(GHCID)
	$(GHCID)
