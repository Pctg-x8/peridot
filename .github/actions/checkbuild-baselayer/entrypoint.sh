#!/bin/bash -xe

export CARGO_TARGET_DIR=$GITHUB_WORKSPACE/.buildcache/target
export CARGO_HOME=$GITHUB_WORKSPACE/.buildcache

cargo check --verbose --features=bedrock/VK_EXT_debug_report
