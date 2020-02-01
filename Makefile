.DEFAULT_GOAL := help

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build all components
	@dune build -j8 ./bin/zkpow/main.exe

run: ## Build and execute the main binary
	@dune exec ./bin/zkpow/main.exe

install: ## Install all components
	@dune install 

test: build ## Execute tests
	@dune runtest

doc: build ## Build documentation
	@opam install odoc
	@dune build @doc
  
clean: ## Clean dune artifacts
	@dune clean


MYUID = $(shell id -u)
DOCKERNAME = zkpowbuilder-$(MYUID)

ifeq ($(DUNE_PROFILE),)
DUNE_PROFILE := dev
endif

TMPDIR ?= /tmp

ifeq ($(USEDOCKER),TRUE)
 $(info INFO Using Docker Named $(DOCKERNAME))
 WRAP = docker exec -it $(DOCKERNAME)
 WRAPAPP = docker exec --workdir /home/opam/app -t $(DOCKERNAME)
else
 $(info INFO Not using Docker)
 WRAP =
endif


containerstart: ## Restarts codabuilder
	@./scripts/container.sh restart
