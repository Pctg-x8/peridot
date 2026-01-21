use std::path::PathBuf;

/*
これの使い方

1. nuget restoreする
2. nugetパッケージのruntimes以下にあるdllをexeと同じディレクトリに置く
3. coreディレクトリにいる状態で cargo run -p editor-winrt-bindgen
*/
fn main() {
    let win2d_canvas_md = nuget_winmd_path(
        "Microsoft.Graphics.Win2D.1.3.2",
        "uap10.0",
        "Microsoft.Graphics.Canvas",
    );
    let appsdk_ui_md = nuget_winmd_path(
        "Microsoft.WindowsAppSDK.1.7.260114001",
        "uap10.0.18362",
        "Microsoft.UI",
    );

    windows_bindgen::bindgen([
        "--out",
        "src/bindgen.rs",
        "--in",
        "default",
        win2d_canvas_md.to_str().unwrap(),
        appsdk_ui_md.to_str().unwrap(),
        "--reference",
        "windows",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.EffectOptimization",
        "Microsoft.Graphics.Canvas.Effects.GaussianBlurEffect",
    ])
    .unwrap();
}

fn nuget_winmd_path(package_fullname: &str, lib_platform: &str, md_name: &str) -> PathBuf {
    std::env::current_dir()
        .expect("no current dir")
        .join(format!(
            ".nuget/{package_fullname}/lib/{lib_platform}/{md_name}.winmd"
        ))
}
