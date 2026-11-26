# ps1 devenv init script(ref: https://www.warpbuild.com/guides/visual-studio-build-tools-github-actions

$ErrorActionPreference = "Stop"

$vswhere = "${Env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"

$vspath = & $vswhere -latest -property installationPath
$envScript = Join-Path $vspath "Common7\Tools\VSDevCmd.bat"
foreach ($setLine in $(cmd /c "`"${envScript}`" -no_logo && set")) {
    if ($setLine -match '^(?<name>[A-Za-z_][A-Za-z0-9_()]*)=(?<value>.*)$') {
        Set-Item -Path env:$($Matches.name) -Value $Matches.value
    } else {
        Write-Host "warn: unrecognized line found"
    }
}

Write-Host "visual studio devenv exported"
