# update root workspace
Write-Host "* Updating root workspace..."
cargo update

# update tools workspace
Write-Host "* Updating tools workspace..."
try {
    Push-Location tools
    cargo update
}
finally {
    Pop-Location
}

# update examples
foreach ($f in (Get-ChildItem examples -Filter Cargo.toml -Recurse)) {
    Write-Host "* Updating examples $($f.Directory.Name)"
    try {
        Push-Location $f.DirectoryName
        cargo update
    }
    finally {
        Pop-Location
    }
}
