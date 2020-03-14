#!/bin/bash -xe

cd $INPUT_PATH

export CARGO_TARGET_DIR=$GITHUB_WORKSPACE/.buildcache/target
export CARGO_HOME=$GITHUB_WORKSPACE/.buildcache

test_or_check() {
    if `/find-test-code.sh`; then echo "test"; else echo "check"; fi
}

for c in */Cargo.toml; do
    (cd $(dirname $c) && cargo `test_or_check` --verbose)
done
