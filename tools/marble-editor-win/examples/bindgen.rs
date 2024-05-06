/*
これの使い方

1. Win2Dのnugetパッケージを落としてくる
2. lib/uap10.0/Microsoft.Graphics.Canvas.winmdをmarble-editor-win直下に置く
3. runtimes/win-64/native/Microsoft.Graphics.Canvas.dllをexeと同じディレクトリに置く
4. cargo run --example bindgen
5. IGraphicsEffectとIGraphicsEffectSourceはwindows-rsのものを使いたいのでsrc/bindgen.rsの該当部分を直接書き換える（インターフェイス宣言じゃなくてuseにする）
*/
fn main() {
    windows_bindgen::bindgen([
        "--out",
        "src/bindgen.rs",
        "--config",
        "flatten",
        "--in",
        "Microsoft.Graphics.Canvas.winmd",
        "--filter",
        "Microsoft.Graphics.Canvas.Effects.GaussianBlurEffect",
    ])
    .unwrap();
}
