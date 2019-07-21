
$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

$Tools = @("archiver","shaderbuild")

try {
    Push-Location
    foreach ($path in $Tools) {
        Write-Host "Building: $path"
        Set-Location $ScriptPath/$path; cargo build --release
    }
}
finally { Pop-Location }