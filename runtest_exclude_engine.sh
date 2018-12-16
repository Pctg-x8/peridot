#!/bin/bash

shopt -s extglob

# for first branch
for c in */Cargo.toml
do (cd $(dirname $c) && cargo test --verbose) || exit 1; done

# for second branch
for c in !(extras|cradle)/*/Cargo.toml
do (cd $(dirname $c) && cargo test --verbose) || exit 2; done

