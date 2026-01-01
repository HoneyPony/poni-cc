#!/bin/bash
# This is like 'compile.sh' except it doesn't use cargo to invoke the compiler.

if ! [[ -f "$1" ]]; then
    echo 'please provide an existing file'
    exit 1
fi

fname="$(basename $1)"
fname="${fname%.c}"

set -e

# preprocess
gcc -E -P "$1" -o "/tmp/$fname.i"

# compile
./target/debug/poni-cc-cli -i "/tmp/$fname.i" -o "/tmp/$fname.s"

# assemble & link
gcc "/tmp/$fname.s" -o "${1%.c}"