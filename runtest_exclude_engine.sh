#!/bin/bash

shopt -s extglob

# for first branch
for c in !(examples)/Cargo.toml
do (cd $(dirname $c) && cargo test --verbose) || exit 1; done

# for second branch
for c in !(extras|cradle|examples)/*/Cargo.toml
do (cd $(dirname $c) && cargo test --verbose) || exit 2; done

