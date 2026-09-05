$ErrorActionPreference = "Stop"
$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path

function Update-Workspace([string] $path) {
    Write-Host -NoNewline "* Updating "
    Write-Host -NoNewline $path -ForegroundColor Yellow
    Write-Host "..."

    try {
        Push-Location $path
        cargo update
    }
    finally {
        Pop-Location
    }
}

Update-Workspace $ProjectRoot
Update-Workspace (Join-Path $ProjectRoot tools)

# examples
Get-ChildItem (Join-Path $ProjectRoot examples) -Filter Cargo.toml -Recurse  | ForEach-Object { Update-Workspace $_.DirectoryName }

# editor
Update-Workspace (Join-Path $ProjectRoot editor win)
Update-Workspace (Join-Path $ProjectRoot editor mac)
Update-Workspace (Join-Path $ProjectRoot editor linux)
