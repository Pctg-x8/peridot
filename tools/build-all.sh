#!/bin/bash -e

SCRIPT_ROOT=$(dirname $0)

(
    cd $SCRIPT_ROOT
    for subdir in *; do
        [ -f $subdir/Cargo.toml ] && (cd $subdir; echo "Building $subdir"; cargo build --release)
    done
)
