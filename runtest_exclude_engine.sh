#!/bin/bash

shopt -s extglob

# for first branch
for c in !(examples|vertex-processing-pack|vg|mmdloader|gltf-loader)/Cargo.toml
do (echo testing $c; cd $(dirname $c) && cargo test --verbose) || exit 1; done

# for second branch
for c in !(extras|cradle|examples)/!(shaderbuild)/Cargo.toml
do (echo testing $c; cd $(dirname $c) && cargo test --verbose) || exit 2; done

# for peridot-dependent modules
(echo checking vertex-processing-pack; cd vertex-processing-pack && cargo check --verbose --features=bedrock/VK_EXT_debug_report) || exit 3
(echo checking vg; cd vg && cargo check --verbose --features=bedrock/VK_EXT_debug_report) || exit 3
(echo checking mmdloader; cd mmdloader && cargo check --verbose --features=bedrock/VK_EXT_debug_report) || exit 3
(echo checking gltf-loader; cd gltf-loader && cargo check --verbose --features=bedrock/VK_EXT_debug_report) || exit 3
# for peridot-dependent tools
(echo checking tools/shaderbuild; cd tools/shaderbuild && cargo check --verbose --features=bedrock/VK_EXT_debug_report) || exit 4

echo check engine
cargo check --verbose --features=bedrock/VK_EXT_debug_report

