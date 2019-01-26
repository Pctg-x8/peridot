
$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path
$ToolPath = "$ScriptPath/../../target/release"

try {
    Push-Location; Set-Location $ScriptPath/assets
    & $ToolPath/peridot-shaderbuild shaders/interior.csh
}
finally { Pop-Location }

