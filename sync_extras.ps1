function CloneOrUpdateGitRepository([string]$url, [string]$into) {
    if (!(Test-Path -Path $into)) {
        git clone $url $into
    } else {
        $local:OriginLocation = Get-Location
        Set-Location $into; git pull -ff; Set-Location $local:OriginLocation
    }
}

New-Item extras -ItemType Directory -Force | Out-Null
CloneOrUpdateGitRepository https://github.com/pcwalton/pathfinder extras/pathfinder
