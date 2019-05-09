
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
$Env:PATH = Append-Variable $Env:PATH ";" "$(Get-Location)\android-build\bin"
$Env:CC = "$(Get-Location)\android-build\bin\aarch64-linux-android-clang.cmd"
$Env:CXX = "$(Get-Location)\android-build\bin\aarch64-linux-android-clang++.cmd"
$Env:LD = "$(Get-Location)\android-build\bin\aarch64-linux-android-clang.cmd"
$Env:AR = "$(Get-Location)\android-build\bin\aarch64-linux-android-ar.exe"
# $Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-I$NdkPath\sysroot\usr\include"
# $Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-I$NdkPath\sysroot\usr\include\aarch64-linux-android"
# $Env:CFLAGS = Append-Variable $Env:CFLAGS " " "--sysroot=$NdkPath\platforms\android-$ANDROID_NDK_PLATFORM_TARGET\arch-arm64"
# $Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-target aarch64-none-linux-android"
$Env:LDFLAGS = Append-Variable $Env:LDFLAGS " " "-L${Env:FREETYPE_LIB_PATH}"
