$ErrorActionPreference = "Stop";

cargo build
winapp create-debug-identity .\target\debug\peridot-marble-editor.exe --no-install
