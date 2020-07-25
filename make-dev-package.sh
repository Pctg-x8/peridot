#!/bin/bash

set -e

usage_exit() {
    echo "usage>$0 [-o OutDirectory] [-c]"
    exit 1
}

SCRIPT_PATH=$(dirname $0)
OUT_DIRECTORY="peridot-sdk"
COMPRESS=0
while getopts oc:h OPT; do
    case $OPT in
        o)
            OUT_DIRECTORY=$OPTARG
            ;;
        c)
            COMPRESS=1
            ;;
        h)
            usage_exit
            ;;
        \?)
            usage_exit
            ;;
    esac
done

mkdir -p $OUT_DIRECTORY || true
rm -rf $OUT_DIRECTORY/*

# Copy cradles
mkdir -p $OUT_DIRECTORY/cradle/windows
rsync -auz --progress $SCRIPT_PATH/cradle/windows $OUT_DIRECTORY/cradle/ --exclude target --exclude userlib.rs --exclude Cargo.toml --exclude Cargo.lock
mkdir -p $OUT_DIRECTORY/cradle/mac
rsync -auz --progress $SCRIPT_PATH/cradle/mac $OUT_DIRECTORY/cradle/ --exclude target --exclude userlib.rs --exclude Cargo.toml --exclude Cargo.lock --exclude build --exclude assets.par --exclude xcuserdata
mkdir -p $OUT_DIRECTORY/cradle/linux
rsync -auz --progress $SCRIPT_PATH/cradle/linux $OUT_DIRECTORY/cradle/ --exclude target --exclude userlib.rs --exclude Cargo.toml --exclude Cargo.lock

# Androidはちょっと複雑(無視するやつが多い......)
mkdir -p $OUT_DIRECTORY/cradle/android
rsync -auz --progress $SCRIPT_PATH/cradle/android $OUT_DIRECTORY/cradle --exclude target --exclude userlib.rs --exclude Cargo.toml --exclude Cargo.lock --exclude apkbuild
rsync -auz --progress $SCRIPT_PATH/cradle/android/apkbuild $OUT_DIRECTORY/cradle/android --exclude AndroidManifest.xml --exclude res --exclude local.properties --exclude .gradle --exclude build.gradle --exclude libpegamelib.so --exclude build --exclude .cxx

# Copy scripts
cp $SCRIPT_PATH/build.ps1 $OUT_DIRECTORY
cp $SCRIPT_PATH/build.sh $OUT_DIRECTORY

# Copy tools(for *nix)
mkdir -p $OUT_DIRECTORY/tools
for f in $(find $SCRIPT_PATH/target/release -name "peridot-*" -type f -perm +a=x); do
    echo "tool detected: $f"
    cp $f $OUT_DIRECTORY/tools/
done

# Compress(if required)
if [ $COMPRESS -ne 0 ]; then zip $OUT_DIRECTORY -o "$OUT_DIRECTORY.zip"; fi
