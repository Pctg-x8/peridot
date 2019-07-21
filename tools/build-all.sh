#!/bin/bash -e

SCRIPT_ROOT=$(dirname $0)

tools=("archiver" "shaderbuild")

(
    cd $SCRIPT_ROOT
    for subdir in ${tools[@]}; do
        [ -f $subdir/Cargo.toml ] && (cd $subdir; echo "Building $subdir"; cargo build --release)
    done
)
