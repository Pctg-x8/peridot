
$OutDirectory = "peridot-sdk"

New-Item $OutDirectory -ItemType Directory -Force | Out-Null
Remove-Item $OutDirectory/* -Recurse -Force

# Copy cradles
robocopy /MIR cradle/windows $OutDirectory/cradle/windows /xd target /xf userlib.rs /xf Cargo.toml /xf Cargo.lock
robocopy /MIR cradle/mac $OutDirectory/cradle/mac /xd target /xf userlib.rs /xf Cargo.toml /xf Cargo.lock
robocopy /MIR cradle/linux $OutDirectory/cradle/linux /xd target /xf userlib.rs /xf Cargo.toml /xf Cargo.lock

# Androidはちょっと複雑(無視するやつが多い......)
robocopy /MIR cradle/android $OutDirectory/cradle/android /xd target /xf userlib.rs /xf Cargo.toml /xf Cargo.lock /xd apkbuild
robocopy /MIR cradle/android/apkbuild $OutDirectory/cradle/android/apkbuild /xf AndroidManifest.xml /xd res /xf local.properties /xd .gradle /xf build.gradle /xf app.iml /xf libpegamelib.so /xd build /xd .cxx

# Copy scripts
Copy-Item build.ps1 $OutDirectory
Copy-Item build.sh $OutDirectory

# Copy tools(for Windows)
New-Item $OutDirectory/tools -ItemType Directory -Force | Out-Null
Copy-Item target/release/peridot-*.exe $OutDirectory/tools
