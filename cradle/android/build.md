How to build Peridot for Android deployment
---

## 必要なもの

- cargo-ndk
  - `cargo install cargo-ndk` でインストールする

## 環境変数

- `NDK_HOME`: Android NDK r20のホーム(toolchainsなどのフォルダがある場所)
- `NDK_PLATFORM_TARGET`: build.gradleのcompileSdkVersionに合わせる。今は28

## コマンドライン

`-Run`/`-r` をつけるとadb経由で実機に転送して自動起動する

### PowerShell

```
$ ./build.ps1 android {UserCode crateのパス} -AssetDirectory {アセット格納パス} -AppPackageID {APKのパッケージID}
```

### bash

```
$ ./build.sh android {UserCode crateのパス} -a {アセット格納パス} -p {APKのパッケージID}
```
