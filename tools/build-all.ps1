
$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

try {
    Push-Location
    foreach ($path in (Get-ChildItem $ScriptPath | Where-Object { Test-Path $ScriptPath/$_/Cargo.toml })) {
        Write-Host "Building: $path"
        Set-Location $ScriptPath/$path; cargo build --release
    }
}
finally { Pop-Location }