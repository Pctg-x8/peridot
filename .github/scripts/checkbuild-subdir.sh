#!/bin/bash -xe

set -o pipefail
shopt -s globstar

test_or_check() {
    if cat ./src/**/*.rs | grep "#\[test\]" > /dev/null
    then
        echo "test"
    else
        echo "check"
    fi
}

for c in **/*/Cargo.toml
do
    pushd $(dirname $c)

    if [ -e ./ci-test.sh ]
    then
        ./ci-test.sh
    else
        cargo `test_or_check` --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
    fi

    popd
done
