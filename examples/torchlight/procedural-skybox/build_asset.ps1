
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
    & glslc shaders/single_scatter_precompute.comp -o shaders/precompute/single_scatter.spv
    & glslc shaders/multiple_scatter_precompute.comp -o shaders/precompute/multiple_scatter.spv
    & glslc shaders/gather_precompute.comp -o shaders/precompute/gather.spv
    & glslc shaders/accum2.comp -o shaders/precompute/accum2.spv
    & glslc shaders/accum3.comp -o shaders/precompute/accum3.spv
}
finally { Pop-Location }
