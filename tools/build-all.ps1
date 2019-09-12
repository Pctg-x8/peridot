
$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

try {
    Push-Location
    foreach ($path in (Get-ChildItem $ScriptPath | Where-Object { Test-Path $_/Cargo.toml })) {
        Write-Host "Building: $(Split-Path $path -Leaf)"
        Set-Location $path; cargo build --release
    }
}
finally { Pop-Location }