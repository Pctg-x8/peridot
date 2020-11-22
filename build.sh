#!/bin/bash

# ./build.sh に続けて,区切りでcradleの名前を指定する

SCRIPT_PATH=$(dirname $0)

TARGETS=$(tr -s ',' ' ' <<< "$1")
shift

for t in $TARGETS; do
    echo "Building for target '$t'..."
    $SCRIPT_PATH/cradle/$t/build.sh $@
    c=$?
    if [[ "$c" != "0" ]]; then exit $c; fi
done
