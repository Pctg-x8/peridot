#!/bin/bash -xe

cd $TARGET_PATH

export CARGO_TARGET_DIR=$GITHUB_WORKSPACE/.buildcache/target
export CARGO_HOME=$GITHUB_WORKSPACE/.buildcache 

for c in */Cargo.toml
do (echo checking $c; cd $(dirname $c) && cargo check --verbose --message-format=json | /cargo-json-gha-translator) || exit $?; done
