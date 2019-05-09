#!/bin/bash

set -e

if [ $# -lt 1 ]; then echo "No enough args"; exit 1; fi

echo "Game Basedir: $1"
echo "Syncing Game Kernel source..."
[ ! -d src/game ] && mkdir -p src/game
rsync -aud --progress $1/src/ src/game
[ -f src/main.rs ] && rm src/main.rs

echo "Building for Target-API-Level $ANDROID_TARGET_API_LEVEL"
if [ ! -d /tmp/peridot/android-toolchain ]; then
	$ANDROID_NDK/build/tools/make-standalone-toolchain.sh --arch=arm64 --platform=android-$ANDROID_TARGET_API_LEVEL \
--install-dir=/tmp/peridot/android-toolchain --force --verbose
fi
export RUSTFLAGS="-C link-arg=--sysroot=/tmp/peridot/android-toolchain/sysroot"
export CC=$ANDROID_NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android$ANDROID_TARGET_API_LEVEL-clang
export CXX=$ANDROID_NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android$ANDROID_TARGET_API_LEVEL-clang++
export LD=$ANDROID_NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android-ld
export AR=$ANDROID_NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android-ar
# export LD=/tmp/peridot/android-toolchain/bin/aarch64-linux-android-ld
# export AR=/tmp/peridot/android-toolchain/bin/aarch64-linux-android-ar
cargo build --features bedrock/VK_EXT_debug_report,bedrock/VK_KHR_android_surface,bedrock/DynamicLoaded --target aarch64-linux-android
[ -d ../../../target/arm64-v8a-linux-android ] && rm -r ../../../target/arm64-v8a-linux-android
mv ../../../target/aarch64-linux-android ../../../target/arm64-v8a-linux-android
