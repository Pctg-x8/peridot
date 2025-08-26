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

run_test() {
    if [ -e ./ci-test.sh ]
    then
        ./ci-test.sh
    else
        cargo `test_or_check` --verbose --message-format=json | $OUTPUT_TRANSLATOR
    fi
}

for c in **/*/Cargo.toml
do
    cd $(dirname $c)
    run_test
done

