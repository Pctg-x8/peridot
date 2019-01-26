
function Append-Variable([string]$org, [string]$delimiter, [string]$v) {
    $paths = $org -split $delimiter
    if ($paths -notcontains $v) {
        $paths += $v
        Write-Output ($paths -join $delimiter)
    }
    else { Write-Output $org }
}

$Env:ANDROID_NDK_PLATFORM_TARGET=21
$NdkPath = "$Env:LocalAppData\Android\Sdk\ndk-bundle"
if (!(Test-Path -Path "android-build")) {
    Write-Host "Initializing Android Build Environment..."
    python $NdkPath\build\tools\make_standalone_toolchain.py --arch=arm64 --api=$Env:ANDROID_NDK_PLATFORM_TARGET --install-dir=android-build
}
else { Write-Host "* Using existing android build environment" }
# $Env:PATH = Append-Variable $Env:PATH ";" "$NdkPath\toolchains\aarch64-linux-android-4.9\prebuilt\windows-x86_64\bin"
$Env:CC = "$(Get-Location)/android-build/bin/aarch64-linux-android-clang.cmd"
$Env:CXX = "$(Get-Location)/android-build/bin/aarch64-linux-android-clang++.cmd"
$Env:LD = "$(Get-Location)/android-build/bin/aarch64-linux-android-clang.cmd"
# $Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-I$NdkPath\sysroot\usr\include"
# $Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-I$NdkPath\sysroot\usr\include\aarch64-linux-android"
# $Env:CFLAGS = Append-Variable $Env:CFLAGS " " "--sysroot=$NdkPath\platforms\android-$ANDROID_NDK_PLATFORM_TARGET\arch-arm64"
# $Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-target aarch64-none-linux-android"
