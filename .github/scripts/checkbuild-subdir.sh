#!/bin/bash -xe

set -o pipefail
shopt -s globstar

test_or_check() {
    for f in ./src/**/*.rs
    do
        if grep "#\[test\]" $f > /dev/null
        then
            echo "test"
            exit
        fi
    done

    echo "check"
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

