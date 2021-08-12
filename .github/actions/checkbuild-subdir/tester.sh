#!/bin/bash -xe

cd $TARGET_PATH

export CARGO_TARGET_DIR=$GITHUB_WORKSPACE/.buildcache/target
export CARGO_HOME=$GITHUB_WORKSPACE/.buildcache

test_or_check() {
    if `/find-test-code.sh`; then echo "test"; else echo "check"; fi
}
run_test() {
    if $(find . -name ci-test.sh | grep .); then ./ci-test.sh; else cargo `test_or_check` --verbose --message-format=json | /cargo-json-gha-translator; fi
}

for c in */Cargo.toml; do
    (cd $(dirname $c) && run_test) || exit $?
done
