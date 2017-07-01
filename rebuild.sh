#!/bin/sh
# Rebuild the boot image and primitives.go.

# This can be used to get access to new primitives written in Go. See
# compiler/primitives.scm.

set -ex

go build
pushd compiler
time ../conscheme -boot conscheme.image.pre-built bytecode-compile
../conscheme -boot conscheme.image genprim > ../vm/primitives.go
cp -f conscheme.image conscheme.image.pre-built
popd
go build
