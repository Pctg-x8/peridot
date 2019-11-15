#!/bin/bash

set -e

SCRIPT_PATH=$(dirname $0)
ENTRY_TY_NAME="Game"
CARGO_SUBCOMMAND="build"
unset PERIDOT_EXTERNAL_ASSET_PATH
while [ $# -gt 0 ]; do
    case "$1" in
        "--EntryTyName" | "-e")
            ENTRY_TY_NAME=$2
            shift 2
            ;;
        "--Run" | "-r")
            CARGO_SUBCOMMAND="run"
            shift
            ;;
        "-AssetDirectory" | "-a")
            export PERIDOT_EXTERNAL_ASSET_PATH=$(realpath $2)
            shift 2
            ;;
        *)
            if [[ ! -v USERLIB_DIRECTORY ]]; then USERLIB_DIRECTORY=$1; fi
            shift
            ;;
    esac
done
if [[ ! -v USERLIB_DIRECTORY ]]; then echo "Error: User Game Project Directory required"; exit 1; fi

. $SCRIPT_PATH/../common.sh

PACKAGE_NAME=`find_package_names $USERLIB_DIRECTORY/Cargo.toml | head -n 1`
echo -e "🛠  Building Project \e[1;36m$PACKAGE_NAME\e[m for \e[33mLinux\e[m Deployment..."

USERLIB_PATH=`realpath $USERLIB_DIRECTORY`
gen_manifest $PACKAGE_NAME $USERLIB_PATH $SCRIPT_PATH/Cargo.template.toml > $SCRIPT_PATH/Cargo.toml
echo -e "//! Auto Generated by build script\n\npub use ${PACKAGE_NAME//-/_}::$ENTRY_TY_NAME as Game;" > $SCRIPT_PATH/src/userlib.rs

FEATURES="bedrock/VK_EXT_debug_report"
if [[ -v PERIDOT_EXTERNAL_ASSET_PATH ]]; then FEATURES="$FEATURES,UseExternalAssetPath"; fi
(cd $SCRIPT_PATH; cargo $CARGO_SUBCOMMAND --features $FEATURES --target x86_64-unknown-linux-gnu)
