CLI_BINARY ?= "src/app/zkpow/main.exe"

DOCKER_HUB_PROFILE ?= tylersmith

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

##
## Docker-based toolchain
##

build-container-stage-1: ## Create stage1 of Docker build container
	@docker build -t $(DOCKER_HUB_PROFILE)/zkpow-build-stage1 -f dockerfiles/Dockerfile.build-stage1 .

build-container: ## Create Docker build container
	@docker build -t $(DOCKER_HUB_PROFILE)/zkpow-build -f dockerfiles/Dockerfile.build .

##
## Help and Phonys
##
.DEFAULT_GOAL := help

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

phonys:
	@cat Makefile | egrep '^\w.*:' | sed 's/:.*/ /' | awk '{print $1}' | grep -v myprocs | sort | xargs

.PHONY: build clean doc help install install_deps phonys run test