#!/bin/bash

set -e

SCRIPT_PATH=$(dirname $0)
ENTRY_TY_NAME="Game"
CARGO_SUBCOMMAND="build"
AFTER_RUN=0
unset ASSET_PATH
while [ $# -gt 0 ]; do
    case "$1" in
        "--EntryTyName" | "-e")
            ENTRY_TY_NAME=$2
            shift 2
            ;;
        "--Run" | "-r")
            AFTER_RUN=1
            shift
            ;;
        "-AssetDirectory" | "-a")
            ASSET_PATH=$(realpath $2)
            shift 2
            ;;
        *)
            if [ -z ${USERLIB_DIRECTORY+x} ]; then USERLIB_DIRECTORY=$(realpath $1); fi
            shift
            ;;
    esac
done
if [ -z ${USERLIB_DIRECTORY+x} ]; then echo "Error: User Game Project Directory required"; exit 1; fi
if [ -z ${ASSET_PATH+x} ]; then echo "Error: AssetDirectory required"; exit 1; fi

echo "Building Assets..."
TOOL_PATH=`realpath $SCRIPT_PATH`/../../target/release
(cd `realpath $ASSET_PATH`; for path in **/*.csh; do $TOOL_PATH/peridot-shaderbuild $path; done)

echo "Syncing Userlib Source from $USERLIB_DIRECTORY..."
mv $SCRIPT_PATH/src/userlib/glib.rs $SCRIPT_PATH/src/userlib/lib.rs 2>/dev/null || true
rsync $USERLIB_DIRECTORY/src/ $SCRIPT_PATH/src/userlib --delete -ac --progress
mv $SCRIPT_PATH/src/userlib/lib.rs $SCRIPT_PATH/src/userlib/glib.rs
echo -e "//! Auto Generated by build script\n\nmod glib; pub use self::glib::$ENTRY_TY_NAME as Game;" > $SCRIPT_PATH/src/userlib.rs

echo "Packaging Assets..."
$SCRIPT_PATH/../../target/release/peridot-archiver new $ASSET_PATH -o $SCRIPT_PATH/peridot-cradle/assets.par -b $ASSET_PATH/

echo "Building GameCore >> App Bundle..."
FEATURES="bedrock/VK_EXT_debug_report,bedrock/VK_MVK_macos_surface"
(cd $SCRIPT_PATH; cargo $CARGO_SUBCOMMAND --features $FEATURES && xcodebuild -project peridot-cradle/peridot-cradle.xcodeproj -configuration Debug build)

if [ $AFTER_RUN -ne 0 ]; then
    lldb -o run $SCRIPT_PATH/peridot-cradle/build/Debug/peridot-cradle.app
fi
