param(
    [parameter(Mandatory=$true, HelpMessage="Target Platform to build")][String[]]$Targets,
    [parameter(Mandatory=$false, ValueFromRemainingArguments=$true)]$PassingArgs
)

$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

function BuildForTarget([String]$Target) {
    Write-Host "Building for target '$Target'..."
    pwsh -NonInteractive -c "$ScriptPath/cradle/$Target/build.ps1 $PassingArgs"
}

$Targets | ForEach-Object { BuildForTarget($_) }
