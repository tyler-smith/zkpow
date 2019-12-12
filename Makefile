.DEFAULT_GOAL := help
.PHONY: help
help:
        @grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

##
## build
##

LINUX_BIN_PATH = ./target/x86_64-unknown-linux-musl/release/zkpow

linux:
	rustup target add x86_64-unknown-linux-musl
	cargo build --release --target=x86_64-unknown-linux-musl

deploy_linux:
	ssh zkpow -C "mkdir -f /opt/zkpow/bin"
	scp $(LINUX_BIN_PATH) zkpow:/opt/zkpow/bin/zkpow

run:
	# build
	# terraform up
	# deploy
	# run
	# collect/download results
	# terraform down
