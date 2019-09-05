#!/bin/bash

shopt -s extglob

# for first branch
for c in !(examples|vertex-processing-pack|coreutils)/Cargo.toml
do (cd $(dirname $c) && cargo test --verbose) || exit 1; done

# for second branch
for c in !(extras|cradle|examples)/!(shaderbuild)/Cargo.toml
do (cd $(dirname $c) && cargo test --verbose) || exit 2; done

# check engine and other untested modules
(cd tools/shaderbuild && cargo check --verbose)
(cd vertex-processing-pack && cargo check --verbose)
(cd coreutils && cargo check --verbose)
cargo check --verbose --features=bedrock/VK_EXT_debug_report

