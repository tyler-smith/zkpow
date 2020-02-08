.DEFAULT_GOAL := help

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

CLI_BINARY="src/app/zkpow/main.exe"

build: ## Build all components
	@dune build -j8 $(CLI_BINARY)

run: ## Build and execute the main binary
	@dune exec $(CLI_BINARY)

install: ## Install all components
	@dune install 

test: build ## Execute tests
	@dune runtest

doc: build ## Build documentation
	@opam install odoc
	@dune build @doc
  
clean: ## Clean dune artifacts
	@dune clean

install_deps: ## Install dependencies from OPAM
	@git submodule init
	@./scripts/setup-opam.sh