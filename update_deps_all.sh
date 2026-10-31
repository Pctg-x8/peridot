#!/usr/bin/env bash

set -e
PROJECT_ROOT=$(dirname $0)

function update_workspace() {
    local DIR=$1
    echo "* Updating $(tput bold)$DIR$(tput sgr0)..."
    (cd $DIR; cargo update)
}

update_workspace $PROJECT_ROOT
update_workspace $PROJECT_ROOT/tools

# examples
for f in $PROJECT_ROOT/examples/**/Cargo.toml; do update_workspace $(dirname $f); done

# editor
update_workspace $PROJECT_ROOT/editor/win
update_workspace $PROJECT_ROOT/editor/mac
update_workspace $PROJECT_ROOT/editor/linux
