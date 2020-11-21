param(
    [parameter(Mandatory=$true, HelpMessage="User Game Project Directory")][String]$UserlibDirectory,
    [parameter(HelpMessage="An structure name of entry point of the game")][String]$EntryTyName = "Game",
    [switch]$Run = $false,
    [parameter(HelpMessage="Asset Directory")][String]$AssetDirectory,
    [parameter(HelpMessage="Package Bundle ID")][String]$AppPackageID = "com.cterm2.peridot",
    [switch]$Release = $false,
    [parameter(HelpMessage="Additional Rust Features")][String[]]$Features = @(),
    [parameter(HelpMessage="Update Cargo dependencies")][switch]$UpdateDeps = $false,
    [parameter(HelpMessage="Run tests via cargo test")][switch]$RunTests = $false
)

$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

function SkipUntil([scriptblock]$pred = $(throw "Need a predicate")) {
    begin {
        $skip = $false
    }
    process {
        if (-not $skip) {
            $skip = & $pred $_
        }

        if ($skip) {
            $_
        }
    }
    end {}
}
function TakeWhile([scriptblock]$pred = $(throw "Need a predicate")) {
    begin {
        $take = $true
    }
    process {
        if ($take) {
            $take = & $pred $_
        }

        if ($take) { $_ }
    }
    end {}
}
function ParseProjectCrateName([String]$ProjectRoot) {
    $FileContent = (Get-Content $ProjectRoot/Cargo.toml) -split "\n"
    while ($true) {
        $FileContent = $FileContent | SkipUntil { $_.toLower() -eq "[package]" } | Select-Object -Skip 1
        if ($FileContent.count -le 0) { break; }
        $PackageContent = $FileContent | TakeWhile { !$_.startsWith("[") } |
            ForEach-Object { $_ -split "=" | ForEach-Object { New-Object string -ArgumentList $_.trim() } }
        $PackageNameHeader = ($PackageContent | SkipUntil { $_.toLower() -ne "name" })
        if ($PackageNameHeader.count -le 0) { continue; }
        return $PackageNameHeader[0].Trim('"')
    }
    throw "No package name found."
}
function BuildCargoManifestFromTemplate([String]$PackageName) {
    $template = Get-Content $ScriptPath\Cargo.template.toml
    $template = $template.Replace("#%KERNEL_CRATE_NAME%", $PackageName)
    $template = $template.Replace("%KERNEL_CRATE_PATH%", (Resolve-Path $UserlibDirectory).Path.Replace("\", "/"))
    $template | Out-File $ScriptPath\Cargo.toml -Encoding UTF8
}

$PackageName = ParseProjectCrateName($UserlibDirectory)
Write-Host -ForegroundColor White -NoNewLine ">"
Write-Host -ForegroundColor Gray -NoNewLine ">"
Write-Host -ForegroundColor DarkGray -NoNewLine ">"
Write-Host -NoNewLine " Building Project "
Write-Host -ForegroundColor Cyan -NoNewLine $PackageName
Write-Host -NoNewLine " for "
Write-Host -ForegroundColor Yellow -NoNewLine "Win32"
Write-Host " Deployment..."
BuildCargoManifestFromTemplate($PackageName)

$ExternCrateName = $PackageName.Replace("-", "_")
"//! Auto Generated by build script

pub use $ExternCrateName::$EntryTyName as Game;" | Out-File $ScriptPath\src\userlib.rs -Encoding UTF8

$CargoSubcommand = if ($Run) { "run" } elseif ($RunTests) { "test" } else { "build" }
$ActiveFeatures = @("bedrock/VK_KHR_win32_surface") + $Features
$OptFlags = if ($Release) { "--release" } else { "" }
if ($AssetDirectory) {
    $Env:PERIDOT_EXTERNAL_ASSET_PATH = $(Resolve-Path $AssetDirectory).Path
    $ActiveFeatures += "UseExternalAssetPath"
}
$Env:PERIDOT_WINDOWS_APPID = $AppPackageID
try {
    Push-Location
    Set-Location $ScriptPath
    if ($UpdateDeps) { cargo update }
    cargo $CargoSubcommand --features $($ActiveFeatures -join ",") $OptFlags
    if ($LastExitCode -ne 0) {
        $c = $LastExitCode
        Write-Error "cargo has exited with code $c"
        exit $c
    }
}
finally { Pop-Location }
