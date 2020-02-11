#!/bin/bash
set -x # echo on
set -eu

# Keep compile dirs to avoid recompiles
export OPAMKEEPBUILDDIR='true'
export OPAMREUSEBUILDDIR='true'
export OPAMYES=1

set-switch.sh

# Workaround a permissions problem in rpc_parallel .git
chmod -R u+rw ~/.opam

opam install -y dune jbuilder
opam pin add src/external/ppx_optcomp
opam pin add src/external/digestif

# All our ocaml packages
opam switch import src/opam.export
eval $(opam config env)

# Our pins
# opam pin add src/external/async_kernel
opam pin add src/external/snarky

eval $(opam config env)