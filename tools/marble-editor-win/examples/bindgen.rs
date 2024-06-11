use std::path::PathBuf;

/*
これの使い方

1. nuget restoreする
2. nugetパッケージのruntimes以下にあるdllをexeと同じディレクトリに置く
3. cargo run --example bindgen
*/
fn main() {
    let win2d_canvas_md = nuget_winmd_path(
        "Microsoft.Graphics.Win2D.1.2.0",
        "uap10.0",
        "Microsoft.Graphics.Canvas",
    );
    let appsdk_ui_md = nuget_winmd_path(
        "Microsoft.WindowsAppSDK.1.5.240428000",
        "uap10.0.18362",
        "Microsoft.UI",
    );

    windows_bindgen::bindgen([
        "--out",
        "src/bindgen.rs",
        "--in",
        win2d_canvas_md.to_str().unwrap(),
        "--in",
        appsdk_ui_md.to_str().unwrap(),
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.GaussianBlurEffect",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.ICanvasEffect",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.EffectOptimization",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.EffectBorderMode",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.IGaussianBlurEffect",
        "--filter",
        "Microsoft.Graphics.Canvas.ICanvasImage",
        "--filter",
        "Microsoft.Graphics.Canvas.CanvasBufferPrecision",
        "--filter",
        "Microsoft.Graphics.Canvas.ICanvasResourceCreatorWithDpi",
        "--filter",
        "Microsoft.Graphics.Canvas.ICanvasResourceCreator",
        "--filter",
        "Microsoft.Graphics.Canvas.CanvasDevice",
        "--filter",
        "Microsoft.Graphics.Canvas.CanvasDpiRounding",
        "--filter",
        "Microsoft.Graphics.Canvas.CanvasLock",
        "--filter",
        "Microsoft.Graphics.Canvas.CanvasDebugLevel",
        "--filter",
        "Microsoft.Graphics.Canvas.ICanvasDeviceFactory",
        "--filter",
        "Microsoft.Graphics.Canvas.ICanvasDeviceStatics",
        "--filter",
        "Microsoft.Graphics.Canvas.ICanvasDevice",
        "--filter",
        "Microsoft.Graphics.Canvas.ICanvasLock",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.MicaController",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.ISystemBackdropController",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.ISystemBackdropControllerWithTargets",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.MicaKind",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.IMicaController",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.IMicaController2",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.SystemBackdropState",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.SystemBackdropTheme",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.IMicaControllerStatics",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.SystemBackdropConfiguration",
        "--filter",
        "Microsoft.UI.Composition.SystemBackdrops.ISystemBackdropConfiguration",
        "--filter",
        "Microsoft.UI.Composition.ICompositionSupportsSystemBackdrop",
        "--filter",
        "Microsoft.UI.IClosableNotifier",
        "--filter",
        "Microsoft.UI.ClosableNotifierHandler",
        "--filter",
        "Microsoft.UI.WindowId",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.TintEffect",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.ITintEffect",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.ITintEffectStatics",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.BorderEffect",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.IBorderEffect",
        "--filter",
        "Microsoft.Graphics.Canvas.CanvasEdgeBehavior",
    ])
    .unwrap();
}

fn nuget_winmd_path(package_fullname: &str, lib_platform: &str, md_name: &str) -> PathBuf {
    let base = PathBuf::from(std::env::var("APPDATA").expect("no appdata?")).join("NuGet/packages");

    base.join(format!(
        "{package_fullname}/lib/{lib_platform}/{md_name}.winmd"
    ))
}
