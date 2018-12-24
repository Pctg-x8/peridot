#!/bin/bash

# ./build.sh に続けて,区切りでcradleの名前を指定する

TARGETS=$(tr -s ',' ' ' <<< "$1")
shift

for t in $TARGETS; do
    echo "Building for target '$t'..."
    ./cradle/$t/build.sh $@
done
