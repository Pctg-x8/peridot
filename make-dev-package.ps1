param(
    [parameter(HelpMessage = "Output Directory Name")][String]$OutDirectory = "peridot-sdk",
    [parameter(HelpMessage = "Package needs zipped?")][switch]$Compress = $false,
    [parameter(HelpMessage = "Specifiy corresponding peridot branch name")][String]$PeridotBranch = "dev"
)

$ErrorActionPreference = "Stop"
$ScriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path

New-Item $OutDirectory -ItemType Directory -Force | Out-Null
Remove-Item $OutDirectory/* -Recurse -Force

# Copy cradles
robocopy /MIR $ScriptPath/cradle/windows $OutDirectory/cradle/windows /xd target /xf userlib.rs /xf Cargo.toml /xf Cargo.lock
robocopy /MIR $ScriptPath/cradle/mac $OutDirectory/cradle/mac /xd target /xf userlib.rs /xf Cargo.toml /xf Cargo.lock
robocopy /MIR $ScriptPath/cradle/linux $OutDirectory/cradle/linux /xd target /xf userlib.rs /xf Cargo.toml /xf Cargo.lock

# Androidはちょっと複雑(無視するやつが多い......)
robocopy /MIR $ScriptPath/cradle/android $OutDirectory/cradle/android /xd target /xf userlib.rs /xf Cargo.toml /xf Cargo.lock /xd apkbuild
robocopy /MIR $ScriptPath/cradle/android/apkbuild $OutDirectory/cradle/android/apkbuild /xf AndroidManifest.xml /xd res /xf local.properties /xd .gradle /xf build.gradle /xf app.iml /xf libpegamelib.so /xd build /xd .cxx

# Rewrite manifest peridot path
function RewriteCargoManifest([String]$filePath) {
    $base = Get-Content $filePath
    $base = $base.Replace("peridot = { path = `"../..`" }", "peridot = { git = `"https://github.com/Pctg-x8/peridot`", branch = `"$PeridotBranch`" }")
    $base | Out-File $filePath -Encoding UTF8
}
Write-Host "Rewriting Cargo Manifests......"
Write-Host "Peridot Branch = $PeridotBranch"
Get-Item $OutDirectory/cradle/**/Cargo.template.toml | ForEach-Object { RewriteCargoManifest $_.FullName }

# Copy scripts
Copy-Item $ScriptPath/build.ps1 $OutDirectory
Copy-Item $ScriptPath/build.sh $OutDirectory

# Copy tools(for Windows)
New-Item $OutDirectory/tools -ItemType Directory -Force | Out-Null
Copy-Item $ScriptPath/target/release/peridot-*.exe $OutDirectory/tools
Copy-Item $ScriptPath/target/release/peridot.exe $OutDirectory/tools

# Compress(if required)
if ($Compress) {
    Compress-Archive $OutDirectory -DestinationPath "$OutDirectory.zip"
}
