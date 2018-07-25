BIN := bin
EMPTY := .make
GHCID := $(BIN)/ghcid
STACK := stack
STACK_WORK := .stack-work

.DEFAULT_GOAL := build

$(EMPTY):
	mkdir $@

$(EMPTY)/stack-setup: | $(EMPTY)
	$(STACK) setup
	touch $@

$(GHCID): $(EMPTY)/stack-setup
	$(STACK) install ghcid --local-bin-path $(BIN)

.PHONY: build
build: $(EMPTY)/stack-setup
	$(STACK) build --no-run-tests --test

.PHONY: clean
clean:
	rm -f $(BIN)/*
	rm -f $(EMPTY)/*
	rm -fr $(STACK_WORK)

.PHONY: test
test: build
	$(STACK) test

.PHONY: watch
watch: $(GHCID)
	$(GHCID)
