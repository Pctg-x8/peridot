param(
    [parameter(Mandatory=$true, HelpMessage="User Game Project Directory")][String]$UserlibDirectory,
    [parameter(HelpMessage="An structure name of entry point of the game")][String]$EntryTyName = "Game",
    [switch]$Run = $false,
    [parameter(Mandatory=$true, HelpMessage="Asset Directory")][String]$AssetDirectory,
    [parameter(Mandatory=$true, HelpMessage="Package Bundle ID")][String]$AppPackageID
)

$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

function Sync-Userlib([string]$userlibBase, [string]$cradleBase) {
    Write-Host "Syncing Userlib Source from $userlibBase..."
    Rename-Item $cradleBase\src\userlib\glib.rs lib.rs -ErrorAction SilentlyContinue
    robocopy $userlibBase\src $cradleBase\src\userlib /mir
    Rename-Item $cradleBase\src\userlib\lib.rs glib.rs
    "//! Auto Generated by build script

mod glib; pub use self::glib::$EntryTyName as Game;" | Set-Content $cradleBase\src\userlib.rs -Encoding UTF8
}
function RewriteAndroidFiles {
    $template = Get-Content $ScriptPath\apkbuild\app\build-template.gradle
    $template = $template.Replace("**APKAPPID**", "'$AppPackageID'")
    $template = $template.Replace("**ASSETDIR**", "'$((Resolve-Path $AssetDirectory).Path.Replace("\", "\\"))'")
    $template | Set-Content $ScriptPath\apkbuild\app\build.gradle

    $template = Get-Content $ScriptPath\apkbuild\app\src\main\AndroidManifest-template.xml
    $template = $template.Replace("**APKAPPID**", "$AppPackageID")
    $template | Set-Content $ScriptPath\apkbuild\app\src\main\AndroidManifest.xml -Encoding UTF8
}

Sync-Userlib $UserlibDirectory $ScriptPath
robocopy $UserlibDirectory\android-res $ScriptPath\apkbuild\app\src\main\res /mir
RewriteAndroidFiles

# Fix for Android build system's platform triple
Rename-Item $ScriptPath/../../target/arm64-v8a-linux-android aarch64-linux-android -ErrorAction SilentlyContinue

# Build Userlib
$features = "bedrock/VK_EXT_debug_report","bedrock/VK_KHR_android_surface","bedrock/DynamicLoaded"
try {
    Push-Location; Set-Location $ScriptPath
    . $ScriptPath/env.ps1
    $Env:RUSTFLAGS="-C link-arg=--sysroot=$(Get-Location)/android-build/sysroot"
    cargo build --features $($features -join ",")
    if ($LASTEXITCODE -ne 0) { throw """cargo build"" failed with code $LASTEXITCODE" }
}
finally { Pop-Location }

# Fix for Android build system's platform triple
Rename-Item $ScriptPath/../../target/aarch64-linux-android arm64-v8a-linux-android

Write-Host "Building APK file..."
try {
    Push-Location; Set-Location $ScriptPath/apkbuild
    ./gradlew assembleDebug
    if ($LASTEXITCODE -ne 0) { throw """./gradlew assembleDebug"" failed with code $LASTEXITCODE" }
}
finally { Pop-Location }

function RunADB([parameter(ValueFromRemainingArguments=$true)]$args) {
    & "$Env:ANDROID_HOME\platform-tools\adb" @args
}

if ($Run) {
    # Run on Android (require to connection)
    try {
        Push-Location
        Set-Location $ScriptPath/apkbuild
        RunADB uninstall $AppPackageID
        RunADB install app/build/outputs/apk/debug/app-debug.apk
        RunADB shell am start -n $AppPackageID/android.app.NativeActivity
    }
    finally { Pop-Location }
}
