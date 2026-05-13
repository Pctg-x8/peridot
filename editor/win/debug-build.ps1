$ErrorActionPreference = "Stop";
$PSNativeCommandUseErrorActionPreference = $true;

# cargo build --features enable-profiling
cargo build
winapp create-debug-identity .\target\debug\peridot-marble-editor.exe --no-install
