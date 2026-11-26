
$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path
$ToolPath = "$ScriptPath/../../tools/target/release"

try {
    Push-Location; Set-Location $ScriptPath/assets
    foreach ($code in (Get-ChildItem shaders/*.csh)) {
        & $ToolPath/peridot-shaderbuild $code
    }
}
finally { Pop-Location }

