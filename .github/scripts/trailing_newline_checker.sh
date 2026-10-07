#!/bin/bash

shopt -s globstar

EXIT=0
for f in **/*.{rs,toml,sh,ps1}
do
    if [ ! "$(tail -c1 $f | hexdump -ve '/1 "%02X"')" = "0A" ]
    then
        echo "::error file=$f::Source files must have a newline at end of the file"
        EXIT=1
    fi
done

exit $EXIT

