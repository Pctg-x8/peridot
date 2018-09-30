$Env:RUSTFLAGS="-C link-arg=--sysroot=$Env:ANDROID_HOME\ndk-bundle\platforms\android-$Env:ANDROID_NDK_PLATFORM_TARGET\arch-arm64"
Write-Host "Syncing Game Kernel source..."
rm -r src/peridot
cp -r ../../src/* src/
rm src/main.rs
cargo build --features bedrock/VK_EXT_debug_report,bedrock/VK_KHR_android_surface,bedrock/DynamicLoaded
rm -r target/arm64-v8a-linux-android
mv target/aarch64-linux-android target/arm64-v8a-linux-android