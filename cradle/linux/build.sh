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

echo "Syncing Userlib Source from $USERLIB_DIRECTORY..."
mv $SCRIPT_PATH/src/userlib/glib.rs $SCRIPT_PATH/src/userlib/lib.rs 2>/dev/null || true
rsync $USERLIB_DIRECTORY/src/ $SCRIPT_PATH/src/userlib --delete -auz --progress
mv $SCRIPT_PATH/src/userlib/lib.rs $SCRIPT_PATH/src/userlib/glib.rs
echo -e "//! Auto Generated by build script\n\nmod glib; pub use self::glib::$ENTRY_TY_NAME as Game;" > $SCRIPT_PATH/src/userlib.rs

echo "Building for Linux..."
FEATURES="bedrock/VK_EXT_debug_report,bedrock/VK_KHR_xcb_surface,peridot-vg/use-freetype,peridot-vg/use-fontconfig"
if [[ -v PERIDOT_EXTERNAL_ASSET_PATH ]]; then FEATURES="$FEATURES,UseExternalAssetPath"; fi
(cd $SCRIPT_PATH; cargo $CARGO_SUBCOMMAND --features $FEATURES)
