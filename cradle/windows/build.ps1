param([parameter(Mandatory=$true, HelpMessage="User Game Project Directory")][String]$UserlibDirectory)

$ErrorActionPreference = "Stop"

Write-Host "Syncing Userlib Source from $UserlibDirectory..."
robocopy $UserlibDirectory\src .\src\userlib /mir
cargo build --features bedrock/VK_EXT_debug_report,bedrock/VK_KHR_win32_surface

