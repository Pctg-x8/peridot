$ErrorActionPreference = "Stop";
$PSNativeCommandUseErrorActionPreference = $true;

cargo build --features enable-profiling
winapp create-debug-identity .\target\debug\peridot-marble-editor.exe --no-install
