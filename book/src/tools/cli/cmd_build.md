# Peridot CLI: Build subcommand

```sh
$ peridot build <game lib directory> -p <cradle> <options...>
```

詳細なオプションについては `peridot help build` を参照してください。

## &lt;cradle&gt;

利用するcradleを指定します。具体的にはcradleディレクトリ以下にあるフォルダを指定できます。

## &lt;game lib directory&gt;

Peridotでは自身のゲームをライブラリクレートとして作成します。詳しい作り方はexamples/image-planeやtapfxを参考にしてください。
機能が足りないのでまだほぼVulkanの知識が必要になります。逆に言うとVulkan coreでできることは全部Peridotでできます。
このパラメータでそのライブラリクレートのルートディレクトリを指定します。

## &lt;options&gt;

オプションを指定します。
1文字のオプションは連続して使用することができます（例: `-ru`）。

### `-r`

ビルド後に起動します。Android cradleを利用する場合はUSBデバッグで接続した端末に上書きインストールを行い、起動します。

### `-f <features...>`

Rustのfeatureを,区切りで指定します。

### `-a <directory>`

アセットローダのベースディレクトリを指定します。mac cradleの場合はBundlesに含むデフォルトのアーカイブファイル(par)を生成する際のベースディレクトリとなります。

### `-u`

cradleディレクトリにあるCargo.lockファイルを更新します（cradleディレクトリでcargo updateを発行するのと同じです）。
ビルドがうまく行かなかった場合にこれをつけると成功することがあります。

### `--appid <package_id>`

パッケージIDを指定します。
Windows cradleの場合はウィンドウクラスのプレフィックスに、Android cradleの場合はそのままパッケージIDとして利用されます。

### `-e <type_name>`

ゲームのエントリポイントとして使用する型名を指定します。
デフォルトは `Game` で、ここで指定する型は以下に示す条件を満たしている必要があります。

```rust
// 次の2つのトレイトを実装している必要があります。
impl<A: peridot::NativeLinker> peridot::EngineEvents<A> for Game<A> {
    // ...
}
impl<A> peridot::FeatureRequests for Game<A> {
    // ...
}

// 次の関連定数を持っている必要があります。
impl<A> Game<A> {
    const NAME: &'static str = "<Game Title>";
    const VERSION: (u32, u32, u32) = (0, 1, 0); // major, minor, patch versions
}
```

## その他

アセットの自動ビルドはこのスクリプトでは行われないため、アセットは別途手動でビルドする必要があります
（ここのプラットフォーム非依存なベストプラクティスが思い浮かばない......）。

examples以下の場合、assetsフォルダ以下がアセットディレクトリとなっており、直下にmakefileがある場合はmakeでビルド可、
それ以外の場合は手動でperidot-shaderbuild / glslcなどを使ってビルドする必要があります。

また、ビルドにはtoolsのビルドが必要になります。これはtoolsディレクトリ以下にある `build-all.ps1` もしくは `build-all.sh` を使うことですべて一括でビルドすることができます。

## 例

```sh
$ peridot build examples/image-plane -p windows -a examples/image-plane/assets --appid jp.ct2.peridot.examples.image_plane -r
```

