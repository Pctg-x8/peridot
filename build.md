Build Instruction
---

(暫定版資料)
Peridot自体は単純なライブラリの集合なので、これに対して特別なビルド手順というものはありません。Cargo.tomlで依存定義すれば使えます。

Peridot上で動くゲームはライブラリクレートとして作成し、それらを対象プラットフォーム向けに作られたcradleクレートの依存として指定してビルドすることで実行可能ファイルを作ることができる形になっています。ここでは、そのビルド手順をある程度自動化してくれるスクリプトの解説をします。

## Build Script for PowerShell

Windows環境の場合は基本的にはこれを使用します。

```
$ build.ps1 <cradle> <game lib directory> <options...>
```

### &lt;cradle&gt;

利用するcradleを,区切りで指定します。具体的にはcradleディレクトリ以下にあるフォルダを指定できます。
クロスコンパイルは対応してるかだいぶ怪しいので（たぶんtarget指定してないのでできない気がする）、基本的にはクロスコンパイルはできないものと思ってください。

### &lt;game lib directory&gt;

Peridotでは自身のゲームをライブラリクレートとして作成します。詳しい作り方はexamples/image-planeやtapfxを参考にしてください。機能が足りないのでまだほぼVulkanの知識が必要になります。逆に言うとVulkan coreでできることは全部Peridotでできます。
このパラメータでそのライブラリクレートのルートディレクトリを指定します。

### &lt;options&gt;

オプションを指定します。

* `-Run`: ビルド後に起動します。Android cradleを利用する場合はUSBデバッグで接続した端末に上書きインストールを行い、起動します。
* `-Features <features...>`: Rustのfeatureを,区切りで指定します。
* `-AssetDirectory <directory>`: アセットローダのベースディレクトリを指定します。mac cradleの場合はBundlesに含むデフォルトのアーカイブファイル(par)を生成する際のベースディレクトリとなります。
* -UpdateDeps: cradleディレクトリにあるCargo.lockファイルを更新します（cradleディレクトリでcargo updateを発行するのと同じです）。ビルドがうまく行かなかった場合にこれをつけると成功することがあります。
* `-RunTests` / `-RunChecks`: CI用。それぞれ `cargo test` と `cargo check` がcradleに対して走ります。
* `-AppPackageID <package_id>`: パッケージIDを指定します。Windows cradleの場合はウィンドウクラスのプレフィックスに、Android cradleの場合はそのままパッケージIDとして利用されます。
* `-EntryTyName <type_name>`: ゲームのエントリポイントとして使用する型名を指定します。デフォルトは `Game` で、ここで指定する型は `peridot::EngineEvents` および `peridot::FeatureRequests` を実装している必要があり、 `const NAME: &'static str` と `const VERSION: (u32, u32, u32)` を関連定数として持っている必要があります。

## Build Script for *nix Shells

macOS / Linuxの場合はこちらを使用します。

```
$ build.sh <cradle> <game lib directory> <options...>
```

ファイル名とoptions以外はPowerShellのものと同じです。

### &lt;options&gt;

オプションを指定します。

* `-r` / `--Run`: ビルド後に起動します。Android cradleを利用する場合はUSBデバッグで接続した端末に上書きインストールを行い、起動します。
* `-f` / `--Features <features...>`: Rustのfeatureを,区切りで指定します。
* `-a` / `--AssetDirectory <directory>`: アセットローダのベースディレクトリを指定します。mac cradleの場合はBundlesに含むデフォルトのアーカイブファイル(par)を生成する際のベースディレクトリとなります。
* `-u` / `--UpdateDeps`: cradleディレクトリにあるCargo.lockファイルを更新します（cradleディレクトリでcargo updateを発行するのと同じです）。ビルドがうまく行かなかった場合にこれをつけると成功することがあります。
* `-p` / `--AppPackageID <package_id>`: パッケージIDを指定します。Windows cradleの場合はウィンドウクラスのプレフィックスに、Android cradleの場合はそのままパッケージIDとして利用されます。
* `-e` / `--EntryTyName <type_name>`: ゲームのエントリポイントとして使用する型名を指定します。デフォルトは `Game` で、ここで指定する型は `peridot::EngineEvents` および `peridot::FeatureRequests` を実装している必要があり、 `const NAME: &'static str` と `const VERSION: (u32, u32, u32)` を関連定数として持っている必要があります。

## その他

アセットの自動ビルドはこのスクリプトでは行われないため、アセットは別途手動でビルドする必要があります（ここのプラットフォーム非依存なベストプラクティスが思い浮かばない......）。
examples以下の場合、assetsフォルダ以下がアセットディレクトリとなっており、直下にmakefileがある場合はmakeでビルド可、それ以外の場合は手動でperidot-shaderbuild / glslcなどを使ってビルドする必要があります。

また、macやアセットビルドにはtoolsのビルドが必要になります。これはtoolsディレクトリ以下にある `build-all.ps1` もしくは `build-all.sh` を使うことですべて一括でビルドすることができます。

## 例

```
$ build.ps1 windows examples/image-plane -AssetDirectory examples/image-plane/assets -AppPackageID jp.ct2.peridot.examples.image_plane -Run
```
