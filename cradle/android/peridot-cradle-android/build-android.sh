#!/bin/bash

set -e

if [ $# -lt 1 ]; then echo "No enough args"; exit 1; fi

echo "Game Basedir: $1"
echo "Syncing Game Kernel source..."
[ ! -d src/game ] && mkdir -p src/game
rsync -aud --progress $1/src/ src/game
[ -f src/main.rs ] && rm src/main.rs

RUSTFLAGS="-C link-arg=--sysroot=$ANDROID_HOME\ndk-bundle\platforms\android-$ANDROID_NDK_PLATFORM_TARGET\arch-arm64"
if [ ! -d /tmp/peridot/android-toolchain ]; then
	$ANDROID_NDK/build/tools/make-standalone-toolchain.sh --arch=arm64 --platform=android-$ANDROID_NDK_PLATFORM_TARGET \
--install-dir=/tmp/peridot/android-toolchain --force
fi
export CC=/tmp/peridot/android-toolchain/bin/aarch64-linux-android-clang
export AR=/tmp/peridot/android-toolchain/bin/aarch64-linux-android-ar
cargo build --features bedrock/VK_EXT_debug_report,bedrock/VK_KHR_android_surface,bedrock/DynamicLoaded
[ -d ../../../target/arm64-v8a-linux-android ] && rm -r ../../../target/arm64-v8a-linux-android
mv ../../../target/aarch64-linux-android ../../../target/arm64-v8a-linux-android
