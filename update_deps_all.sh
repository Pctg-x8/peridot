#!/usr/bin/env bash

# update root workspace
echo "* Updating root workspace..."
cargo update

# update tools workspace
echo "* Updating tools workspace..."
(cd tools; cargo update)

# update examples
for f in examples/**/Cargo.toml; do
    echo "* Updating examples $(dirname $f)..."
    (cd $(dirname $f); cargo update)
done

