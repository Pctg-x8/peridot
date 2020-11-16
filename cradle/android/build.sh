#!/bin/bash

set -e

AFTER_RUN=0
SCRIPT_PATH=$(dirname $0)
ENTRY_TY_NAME="Game"
unset PACKAGE_ID
unset PERIDOT_EXTERNAL_ASSET_PATH
UPDATE_DEPS=0
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
            export PERIDOT_EXTERNAL_ASSET_PATH=$(realpath $2)
            shift 2
            ;;
        "-AppPackageID" | "-p")
            export PACKAGE_ID=$2
            shift 2
            ;;
        "--UpdateDeps" | "-u")
            UPDATE_DEPS=1
            shift
            ;;
        *)
            if [ -z ${USERLIB_DIRECTORY+x} ]; then USERLIB_DIRECTORY=$1; fi
            shift
            ;;
    esac
done
if [ -z ${USERLIB_DIRECTORY+x} ]; then echo "Error: User Game Project Directory required"; exit 1; fi
if [ -z ${PACKAGE_ID+x} ]; then echo "Error: Apk Package ID required"; exit 1; fi

. $SCRIPT_PATH/../common.sh
function gen_android_files() {
	local ASSET_REAL_PATH=`realpath $PERIDOT_EXTERNAL_ASSET_PATH`
	sed -e "s/\*\*APKAPPID\*\*/'$PACKAGE_ID'/g" -e "s/\*\*ASSETDIR\*\*/${ASSET_REAL_PATH//\//\\/}/g" $SCRIPT_PATH/apkbuild/app/build-template.gradle > $SCRIPT_PATH/apkbuild/app/build.gradle
	sed -e "s/\*\*APKAPPID\*\*/$PACKAGE_ID/g" $SCRIPT_PATH/apkbuild/app/src/main/AndroidManifest-template.xml > $SCRIPT_PATH/apkbuild/app/src/main/AndroidManifest.xml
}

PACKAGE_NAME=`find_package_names $USERLIB_DIRECTORY/Cargo.toml | head -n 1`
echo -e "🛠  Building Project \e[1;36m$PACKAGE_NAME\e[m for \e[33mAndroid\e[m Deployment..."

USERLIB_PATH=`realpath $USERLIB_DIRECTORY`
gen_manifest $PACKAGE_NAME $USERLIB_PATH $SCRIPT_PATH/Cargo.template.toml > $SCRIPT_PATH/Cargo.toml
gen_android_files
echo -e "//! Auto Generated by build script\n\npub use ${PACKAGE_NAME//-/_}::$ENTRY_TY_NAME as Game;" > $SCRIPT_PATH/src/userlib.rs

# Merge user-defined xmls within default xmls
rsync -auz $SCRIPT_PATH/apkbuild/app/src/main/res-default/* $SCRIPT_PATH/apkbuild/app/src/main/res
[ -d $USERLIB_DIRECTORY/android-res ] && rsync -a $USERLIB_DIRECTORY/android-res/* $SCRIPT_PATH/apkbuild/app/src/main/res

# mirror extlib
[ -d $USERLIB_DIRECTORY/extlib/android ] && rsync -auz $USERLIB_DIRECTORY/extlib/android/* $SCRIPT_PATH/apkbuild/app/src/main/jniLibs --exclude ".*"

[ -d $SCRIPT_PATH/target/arm64-v8a-linux-android ] && mv $SCRIPT_PATH/target/arm64-v8a-linux-android $SCRIPT_PATH/target/aarch64-linux-android
if [ $UPDATE_DEPS -ne 0 ]; then (cd $SCRIPT_PATH; cargo update); fi
(cd $SCRIPT_PATH; cargo ndk --target aarch64-linux-android --android-platform $NDK_PLATFORM_TARGET -- build --features bedrock/VK_EXT_debug_report,bedrock/VK_KHR_android_surface,bedrock/DynamicLoaded)
[ ! -d $SCRIPT_PATH/apkbuild/app/src/main/jniLibs/arm64-v8a ] && mkdir -p $SCRIPT_PATH/apkbuild/app/src/main/jniLibs/arm64-v8a
mv $SCRIPT_PATH/target/aarch64-linux-android/debug/libpegamelib.so $SCRIPT_PATH/apkbuild/app/src/main/jniLibs/arm64-v8a/

echo -e "🛠  Building APK..."
(cd $SCRIPT_PATH/apkbuild; ./gradlew assembleDebug)

echo -e "💠  $SCRIPT_PATH/apkbuild/app/build/outputs/apk/debug/app-debug.apk"

if [ $AFTER_RUN -ne 0 ]; then
    ADB=$ANDROID_HOME/platform-tools/adb
	(cd $SCRIPT_PATH/apkbuild; $ADB uninstall $PACKAGE_ID; $ADB install app/build/outputs/apk/debug/app-debug.apk && $ADB shell am start -n $PACKAGE_ID/jp.ct2.peridot.NativeActivity)
fi
