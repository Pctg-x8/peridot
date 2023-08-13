#!/bin/bash

shopt -s extglob

# for first branch
for c in !(examples|vertex-processing-pack|vg|base)/Cargo.toml
do (echo testing $c; cd $(dirname $c) && cargo test --verbose) || exit 1; done

# for second branch
for c in !(extras|cradle|examples)/!(shaderbuild)/Cargo.toml
do (echo testing $c; cd $(dirname $c) && cargo test --verbose) || exit 2; done

# for peridot-dependent modules
(echo checking vertex-processing-pack; cd vertex-processing-pack && cargo check --verbose --features=bedrock/VK_EXT_debug_report) || exit 3
(echo checking vg; cd vg && cargo check --verbose --features=bedrock/VK_EXT_debug_report) || exit 3
# for peridot-dependent tools
(echo checking tools/shaderbuild; cd tools/shaderbuild && cargo check --verbose --features=bedrock/VK_EXT_debug_report) || exit 4

echo checking engine
(cd base && cargo check --verbose --features=bedrock/VK_EXT_debug_report)
