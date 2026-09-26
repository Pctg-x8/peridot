#[cfg(windows)]
fn main() {
    // detect msvc installation
    let res = std::process::Command::new(
        std::path::PathBuf::from(
            std::env::var_os("ProgramFiles(x86)").expect("no program files x86"),
        )
        .join("Microsoft Visual Studio/Installer/vswhere.exe"),
    )
    .args(["-latest", "-property", "resolvedInstallationPath"])
    .output()
    .expect("failed");
    if !res.status.success() {
        panic!("vswhere exited with status: {}", res.status);
    }
    let vs_installation = std::path::Path::new(
        std::str::from_utf8(res.stdout.trim_ascii_end()).expect("invalid output from vswhere"),
    );

    // locate win10 sdk
    let mut regkey = core::mem::MaybeUninit::uninit();
    unsafe {
        windows::Win32::System::Registry::RegOpenKeyExW(
            windows::Win32::System::Registry::HKEY_LOCAL_MACHINE,
            // TODO: 32bitマシンの場合はWoW6432Nodeにないので注意（でもサポートする必要あるか？）
            windows::core::w!("SOFTWARE\\WOW6432Node\\Microsoft\\Microsoft SDKs\\Windows\\v10.0"),
            None,
            windows::Win32::System::Registry::KEY_READ,
            regkey.as_mut_ptr(),
        )
        .ok()
        .expect("failed to open registry key")
    };
    let regkey = unsafe { regkey.assume_init() };
    let installation_folder = std::path::PathBuf::from(
        query_regkey_osstr::<256>(regkey, windows::core::w!("InstallationFolder"))
            .expect("failed to read installation folder"),
    );
    let product_version = query_regkey_osstr::<32>(regkey, windows::core::w!("ProductVersion"))
        .expect("failed to read product version");

    // TODO: バージョン番号やプラットフォームは固定値じゃなくて別のところから取得する必要がある
    let vs_buildtool_path = vs_installation.join("VC\\Tools\\MSVC\\14.51.36231\\bin\\Hostx64\\x64");
    let vs_include_path = vs_installation.join("VC\\Tools\\MSVC\\14.51.36231\\include");
    let vs_lib_path = vs_installation.join("VC\\Tools\\MSVC\\14.51.36231\\lib\\x64");
    let ucrt_include_path =
        installation_folder.join(format!("Include\\{}.0\\ucrt", product_version.display()));
    let um_include_path =
        installation_folder.join(format!("Include\\{}.0\\um", product_version.display()));
    let shared_include_path =
        installation_folder.join(format!("Include\\{}.0\\shared", product_version.display()));
    let um_lib_path =
        installation_folder.join(format!("Lib\\{}.0\\um\\x64", product_version.display()));
    let ucrt_lib_path =
        installation_folder.join(format!("Lib\\{}.0\\ucrt\\x64", product_version.display()));
    let win10sdk_bin_path =
        installation_folder.join(format!("bin\\{}.0\\x64", product_version.display()));

    let newenv_path = std::env::var("PATH").unwrap_or_default()
        + &format!(
            ";{};{}",
            vs_buildtool_path.display(),
            win10sdk_bin_path.display()
        );
    let res = std::process::Command::new("nmake")
        .args(["/f", "Makefile.msc", "libsqlite3.lib"])
        .current_dir(
            std::env::current_dir()
                .expect("current_dir")
                .join("source-repo"),
        )
        // Note: cargoかなんかがこれを設定していてMakefile.msc内の条件式がエラーになるので消す
        .env_remove("DEBUG")
        .env("PATH", newenv_path)
        .env(
            "INCLUDE",
            format!(
                "{};{};{};{}",
                vs_include_path.display(),
                ucrt_include_path.display(),
                um_include_path.display(),
                shared_include_path.display()
            ),
        )
        .env(
            "LIB",
            format!(
                "{};{};{}",
                ucrt_lib_path.display(),
                um_lib_path.display(),
                vs_lib_path.display()
            ),
        )
        .status()
        .expect("failed");
    if !res.success() {
        panic!("nmake exited with status: {}", res);
    }

    println!(
        "cargo::rustc-link-search=static={}",
        std::env::current_dir()
            .expect("current_dir")
            .join("source-repo")
            .display()
    );
    println!("cargo::rustc-link-lib=libsqlite3");
}

#[cfg(windows)]
fn query_regkey_osstr<const FAST_PASS_CHAR_COUNT: usize>(
    key: windows::Win32::System::Registry::HKEY,
    name: windows::core::PCWSTR,
) -> windows::core::Result<std::ffi::OsString> {
    let mut buf = Vec::<u16>::with_capacity(FAST_PASS_CHAR_COUNT);
    let mut len = size_of_val(buf.spare_capacity_mut()) as u32;
    let r = unsafe {
        windows::Win32::System::Registry::RegGetValueW(
            key,
            None,
            name,
            windows::Win32::System::Registry::RRF_RT_REG_SZ,
            None,
            Some(buf.spare_capacity_mut().as_mut_ptr().cast()),
            Some(&mut len),
        )
        .ok()
    };
    match r {
        Ok(_) => {
            unsafe {
                buf.set_len((len as usize / 2) - 1);
            }
            return Ok(std::os::windows::ffi::OsStringExt::from_wide(&buf));
        }
        Err(e) if e != windows::Win32::Foundation::ERROR_MORE_DATA.into() => {
            return Err(e);
        }
        _ => (),
    }

    buf.reserve(len as _);
    unsafe {
        windows::Win32::System::Registry::RegGetValueW(
            key,
            None,
            name,
            windows::Win32::System::Registry::RRF_RT_REG_SZ,
            None,
            Some(buf.spare_capacity_mut().as_mut_ptr().cast()),
            Some(&mut len),
        )
        .ok()?;
    }
    unsafe { buf.set_len((len as usize / 2) - 1) };
    Ok(std::os::windows::ffi::OsStringExt::from_wide(&buf))
}
