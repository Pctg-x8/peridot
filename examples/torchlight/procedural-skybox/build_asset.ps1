
$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path
$ToolPath = "$ScriptPath/../../../target/release"

try {
    Push-Location; Set-Location $ScriptPath/assets
    foreach ($code in (Get-ChildItem shaders/*.csh)) {
        & $ToolPath/peridot-shaderbuild $code
    }
    Write-Host "Compiling Precomputation Shaders......"
    & glslc shaders/transmittance_precompute.comp -o shaders/precompute/transmittance.spv
}
finally { Pop-Location }
