
$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

try {
    Push-Location
    $Targets = Get-ChildItem $ScriptPath -Directory -Name | Where-Object { Test-Path $ScriptPath/$_/Cargo.toml }
    foreach ($path in $Targets) {
        Write-Host "Building: $path"
        Set-Location $ScriptPath/$path
        cargo build --release 2>&1 | %{ "$_" }
    }
}
finally { Pop-Location }