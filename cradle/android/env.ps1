
function Append-Variable([string]$org, [string]$delimiter, [string]$v) {
    $paths = $org -split $delimiter
    if ($paths -notcontains $v) {
        $paths += $v
        Write-Output ($paths -join $delimiter)
    }
    else { Write-Output $org }
}

$NdkPath = "$Env:LocalAppData\Android\Sdk\ndk-bundle"
$Env:PATH = Append-Variable $Env:PATH ";" "$NdkPath\toolchains\aarch64-linux-android-4.9\prebuilt\windows-x86_64\bin"
$Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-I$NdkPath\sysroot\usr\include"
$Env:CFLAGS = Append-Variable $Env:CFLAGS " " "-I$NdkPath\sysroot\usr\include\aarch64-linux-android"
