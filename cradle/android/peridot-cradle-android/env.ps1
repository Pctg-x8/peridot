$NdkPath = "$Env:LocalAppData\Android\Sdk\ndk-bundle"
$Env:PATH += ";$NdkPath\toolchains\aarch64-linux-android-4.9\prebuilt\windows-x86_64\bin"
$Env:CFLAGS += " -I$NdkPath\sysroot\usr\include -I$NdkPath\sysroot\usr\include\aarch64-linux-android"