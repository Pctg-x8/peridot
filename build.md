Build Instruction
---

(暫定版資料)
Peridot自体は単純なライブラリの集合なので、これに対して特別なビルド手順というものはありません。Cargo.tomlで依存定義すれば使えます。

Peridot上で動くゲームはライブラリクレートとして作成し、それらを対象プラットフォーム向けに作られたcradleクレートの依存として指定してビルドすることで実行可能ファイルを作ることができる形になっています。ここでは、そのビルド手順をある程度自動化してくれるツールの解説をします。

## Peridot CLI

開発環境の場合は `/target/release/` dev-packageの場合は `/tools/` 以下にあります。
詳細なオプションについては `peridot help build` を参照してください。

```
$ peridot build <game lib directory> -p <cradle> <options...>
```

### &lt;cradle&gt;

利用するcradleを指定します。具体的にはcradleディレクトリ以下にあるフォルダを指定できます。

### &lt;game lib directory&gt;

Peridotでは自身のゲームをライブラリクレートとして作成します。詳しい作り方はexamples/image-planeやtapfxを参考にしてください。
機能が足りないのでまだほぼVulkanの知識が必要になります。逆に言うとVulkan coreでできることは全部Peridotでできます。
このパラメータでそのライブラリクレートのルートディレクトリを指定します。

### &lt;options&gt;

オプションを指定します。

* `-r`: ビルド後に起動します。Android cradleを利用する場合はUSBデバッグで接続した端末に上書きインストールを行い、起動します。
* `-f <features...>`: Rustのfeatureを,区切りで指定します。
* `-a <directory>`: アセットローダのベースディレクトリを指定します。mac cradleの場合はBundlesに含むデフォルトのアーカイブファイル(par)を生成する際のベースディレクトリとなります。
* -u: cradleディレクトリにあるCargo.lockファイルを更新します（cradleディレクトリでcargo updateを発行するのと同じです）。ビルドがうまく行かなかった場合にこれをつけると成功することがあります。
* `--appid <package_id>`: パッケージIDを指定します。Windows cradleの場合はウィンドウクラスのプレフィックスに、Android cradleの場合はそのままパッケージIDとして利用されます。
* `-e <type_name>`: ゲームのエントリポイントとして使用する型名を指定します。デフォルトは `Game` で、ここで指定する型は `peridot::EngineEvents` および `peridot::FeatureRequests` を実装している必要があり、 `const NAME: &'static str` と `const VERSION: (u32, u32, u32)` を関連定数として持っている必要があります。

## その他

アセットの自動ビルドはこのスクリプトでは行われないため、アセットは別途手動でビルドする必要があります（ここのプラットフォーム非依存なベストプラクティスが思い浮かばない......）。
examples以下の場合、assetsフォルダ以下がアセットディレクトリとなっており、直下にmakefileがある場合はmakeでビルド可、それ以外の場合は手動でperidot-shaderbuild / glslcなどを使ってビルドする必要があります。

また、ビルドにはtoolsのビルドが必要になります。これはtoolsディレクトリ以下にある `build-all.ps1` もしくは `build-all.sh` を使うことですべて一括でビルドすることができます。

## 例

```
$ peridot build examples/image-plane -p windows -a examples/image-plane/assets --appid jp.ct2.peridot.examples.image_plane -r
```
