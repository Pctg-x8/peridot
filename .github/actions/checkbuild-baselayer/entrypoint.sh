#!/bin/bash -xe

set -o pipefail

export CARGO_TARGET_DIR=$GITHUB_WORKSPACE/.buildcache/target
export CARGO_HOME=$GITHUB_WORKSPACE/.buildcache

cd base
cargo check --verbose --features=bedrock/VK_EXT_debug_report --message-format=json | /cargo-json-gha-translator
cargo check --verbose --features=bedrock/VK_EXT_debug_report,mt --message-format=json | /cargo-json-gha-translator
