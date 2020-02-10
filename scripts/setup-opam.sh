#!/bin/bash
set -x # echo on
set -eu

# Keep compile dirs to avoid recompiles
export OPAMKEEPBUILDDIR='true'
export OPAMREUSEBUILDDIR='true'
export OPAMYES=1

# Set term to xterm if not set
export TERM=${TERM:-xterm}

# ocaml environment
eval $(opam config env)

SWITCH_BASE='4.07.1'
SWITCH='zkpow-4.07.1-2'
SWITCH_LIST=$(opam switch list -s)


# Check for cached switch
SWITCH_FOUND=false
for val in $SWITCH_LIST; do
  if [ $val == $SWITCH ]; then
    SWITCH_FOUND=true
  fi
done

if [ "$SWITCH_FOUND" = true ]; then
  opam switch set $SWITCH
  eval $(opam config env)
else
  # Build switch from scratch
  opam init
  opam update
  opam switch create $SWITCH $SWITCH_BASE || true
  opam switch $SWITCH
fi

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