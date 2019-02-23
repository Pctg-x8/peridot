param(
    [parameter(Mandatory=$true, HelpMessage="Target Platform to build")][String[]]$Targets,
    [parameter(Mandatory=$false, ValueFromRemainingArguments=$true)]$PassingArgs
)

function BuildForTarget([String]$Target) {
    Write-Host "Building for target '$Target'..."
    Invoke-Expression "./cradle/$Target/build.ps1 $PassingArgs"
}

$Targets | ForEach-Object { BuildForTarget($_) }
