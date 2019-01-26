
function Append-Variable([string]$org, [string]$delimiter, [string]$v) {
    $paths = $org -split $delimiter
    if ($paths -notcontains $v) {
        $paths += $v
        Write-Output ($paths -join $delimiter)
    }
    else { Write-Output $org }
}

$NdkPath = "$Env:LocalAppData\Android\Sdk\ndk-bundle"
if (!(Test-Path "android-build")) {
    Write-Host "Initializing Android BuildTools..."
    python3 $NdkPath\build\tools\make_standalone_toolchain.py --arch arm64 --api $Env:ANDROID_NDK_PLATFORM_TARGET --install-dir android-build
}
$Env:CC = "$(Get-Location)\android-build\bin\clang.cmd"
$Env:CMAKE_C_COMPILER = "$(Get-Location)\android-build\bin\clang.cmd"
$Env:CXX = "$(Get-Location)\android-build\bin\clang++.cmd"
$Env:CMAKE_CXX_COMPILER = "$(Get-Location)\android-build\bin\clang++.cmd"
$Env:LD = "$(Get-Location)\android-build\bin\clang.cmd"
$Env:SYSROOT = "$(Get-Location)\android-build\sysroot"

$Env:PATH = Append-Variable $Env:PATH ";" "$(Get-Location)\android-build\bin"
$Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-I$Env:SYSROOT\include"
$Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-I$Env:SYSROOT\usr\include\aarch64-linux-android"
