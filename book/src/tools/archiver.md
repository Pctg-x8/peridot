# archiver

複数ファイルを一つのファイル（par）にアーカイブします。アーカイブしたファイルはアセットを通して読み込めるようになります。

## subcommands

### extract

アーカイブファイルに格納された指定のファイルをstdoutに展開します。

```sh
$ peridot-archiver extract <archive file> [<asset path>] [--check-integrity]
```

#### `<archive file>`

展開したいアーカイブファイルを指定します。

#### `<asset_path>`

展開したいアーカイブファイル内のファイルへのパスを指定します。

#### `--check-integrity`

これを指定すると、アーカイブファイルを開く際に整合性チェックを行うようになります。

### list

アーカイブファイルに格納されたファイルのパスを表示します。

```sh
$ peridot-archiver list <archive file> [--check-integrity]
```

#### `<archive file>`

表示したいアーカイブファイルを指定します。

#### `--check-integrity`

これを指定すると、アーカイブファイルを開く際に整合性チェックを行うようになります。

### new

指定したファイル、またはディレクトリからアーカイブファイルを生成します。

```sh
$ peridot-archiver new <input file/directory...> [-o <output path>] [-b <base directory>] [-c <compression method>]
```

#### `<input file/directory...>`

アーカイブファイルに格納したいファイルまたはディレクトリを指定します。
複数指定可能です。

#### `-o <output path>`

アーカイブファイルの出力パスを指定します。
指定しなかった場合はstdoutに出力されます。

#### `-b <base directory>`

アーカイブファイルに記載するファイルパスのベースとなるディレクトリを指定します。

例えば、base directoryに `/game/assets` を指定すると、
入力ファイルのパスが `/game/assets/shaders/test.pvp` だった場合はアーカイブファイルには
`shaders/test.pvp` で保存されるようになります。

#### `-c <compression method>`

アーカイブファイルの圧縮メソッドを指定します。指定しなかった場合は圧縮なしで、次のメソッドが利用できます。

* `lz4`: LZ4圧縮
* `zlib`: zlib圧縮
* `zstd11`: zstd圧縮 levelは11固定
