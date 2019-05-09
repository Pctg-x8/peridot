param(
    [parameter(Mandatory=$true, HelpMessage="User Game Project Directory")][String]$UserlibDirectory,
    [parameter(HelpMessage="An structure name of entry point of the game")][String]$EntryTyName = "Game",
    [switch]$Run = $false,
    [parameter(HelpMessage="Asset Directory")][String]$AssetDirectory,
    [parameter(HelpMessage="Package Bundle ID")][String]$AppPackageID = "com.cterm2.peridot"
)

$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "Syncing Userlib Source from $UserlibDirectory..."
Rename-Item $ScriptPath\src\userlib\glib.rs lib.rs -ErrorAction SilentlyContinue
robocopy $UserlibDirectory\src $ScriptPath\src\userlib /mir
Rename-Item $ScriptPath\src\userlib\lib.rs glib.rs
"//! Auto Generated by build script

mod glib; pub use self::glib::$EntryTyName as Game;" | Out-File $ScriptPath\src\userlib.rs -Encoding UTF8
Write-Host "Building Win32 Exe......"
$CargoSubcommand = if ($Run) { "run" } else { "build" }
$Features = "bedrock/VK_EXT_debug_report","bedrock/VK_KHR_win32_surface"
if ($AssetDirectory) {
    $Env:PERIDOT_EXTERNAL_ASSET_PATH = $(Resolve-Path $AssetDirectory).Path
    $Features += "UseExternalAssetPath"
}
$Env:PERIDOT_WINDOWS_APPID = $AppPackageID
try {
    Push-Location
    Set-Location $ScriptPath
    cargo +nightly $CargoSubcommand --features $($Features -join ",")
}
finally { Pop-Location }
