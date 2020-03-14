#!/bin/bash

for f in $(find src -name "*.rs"); do
    grep "#\[test\]" $f > /dev/null && exit 0
done
exit 1
