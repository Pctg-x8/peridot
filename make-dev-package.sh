#!/usr/bin/env bash

set -e
shopt -s globstar

usage_exit() {
    echo "usage>$0 [-o OutDirectory] [-c]"
    exit 1
}

SCRIPT_PATH=$(dirname $0)
OUT_DIRECTORY="peridot-sdk"
PERIDOT_BRANCH="dev"
COMPRESS=0
while getopts o:b:ch OPT; do
    case $OPT in
        o)
            OUT_DIRECTORY=$OPTARG
            ;;
        b)
            PERIDOT_BRANCH=$OPTARG
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

# Copy builtin assets
mkdir -p $OUT_DIRECTORY/builtin-assets
rsync -auz --progress $SCRIPT_PATH/builtin-assets $OUT_DIRECTORY/ --exclude *.csh --exclude Makefile

# Copy cradles
mkdir -p $OUT_DIRECTORY/cradle/windows
rsync -auz --progress $SCRIPT_PATH/cradle/windows $OUT_DIRECTORY/cradle/ --exclude target --exclude userlib.rs --exclude Cargo.toml --exclude Cargo.lock
mkdir -p $OUT_DIRECTORY/cradle/mac
rsync -auz --progress $SCRIPT_PATH/cradle/mac $OUT_DIRECTORY/cradle/ --exclude target --exclude userlib.rs --exclude Cargo.toml --exclude Cargo.lock --exclude build --exclude assets.par --exclude xcuserdata --exclude rlibs
mkdir -p $OUT_DIRECTORY/cradle/linux
rsync -auz --progress $SCRIPT_PATH/cradle/linux $OUT_DIRECTORY/cradle/ --exclude target --exclude userlib.rs --exclude Cargo.toml --exclude Cargo.lock

# Androidはちょっと複雑(無視するやつが多い......)
mkdir -p $OUT_DIRECTORY/cradle/android
rsync -auz --progress $SCRIPT_PATH/cradle/android $OUT_DIRECTORY/cradle --exclude target --exclude userlib.rs --exclude Cargo.toml --exclude Cargo.lock --exclude apkbuild
rsync -auz --progress $SCRIPT_PATH/cradle/android/apkbuild $OUT_DIRECTORY/cradle/android --exclude AndroidManifest.xml --exclude res --exclude local.properties --exclude .gradle --exclude build.gradle --exclude libpegamelib.so --exclude build --exclude .cxx

# Rewrite manifest peridot path
echo "Rewriting Cargo Manifests......"
echo "Peridot Branch = $PERIDOT_BRANCH"
for f in $OUT_DIRECTORY/cradle/**/Cargo.template.toml; do
    echo "rewriting peridot deps in $f..."
    sed -i.o -e "s/peridot = { path = \"..\\/..\\/base\" }/peridot = { git = \"https:\\/\\/github.com\\/Pctg-x8\\/peridot\", branch = \"${PERIDOT_BRANCH//\/\\\/}\" }/g" $f
    rm $f.o
done

# Select prefer gfind(from findutils)
if type gfind > /dev/null 2>&1; then FIND=gfind; else FIND=find; fi

# Copy tools(for *nix)
mkdir -p $OUT_DIRECTORY/tools
for f in $($FIND $SCRIPT_PATH/target/release -name "peridot-*" -type f -perm /a+x); do
    echo "tool detected: $f"
    cp $f $OUT_DIRECTORY/tools/
done
cp $SCRIPT_PATH/target/release/peridot $OUT_DIRECTORY/tools/

# Compress(if required)
if [ $COMPRESS -ne 0 ]; then zip -r "$OUT_DIRECTORY.zip" $OUT_DIRECTORY; fi
