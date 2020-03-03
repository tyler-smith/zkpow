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
SWITCH="zkpow-$SWITCH_BASE"
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
else
  # Build switch from scratch
  opam init
  opam update
  opam switch create $SWITCH $SWITCH_BASE || true
  opam switch $SWITCH
fi

eval $(opam config env)