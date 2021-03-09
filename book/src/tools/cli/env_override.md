# Environment Overriding

Peridot CLIでは、環境変数を使用することでいくつかのデフォルト設定をオーバーライドできます（主に開発用）。

## `PERIDOT_CLI_CRADLE_BASE`

cradleディレクトリへのパスを指定します。
dev-packageを使用しない場合は必ず指定する必要があります。

指定しない場合は、Peridot CLIのひとつ上のディレクトリにあるcradleディレクトリが使用されます。

## `PERIDOT_CLI_ARCHIVER_PATH`

[Peridot Archiver](../archiver.md)へのパスを指定します。
macOSではビルド中にArchiverを走らせるため、dev-packageを使用しない場合は指定が必須になります。

指定しない場合は、Peridot CLIと同じディレクトリにあるperidot-archiverが使用されます。

## `PERIDOT_CLI_BUILTIN_ASSETS_PATH`

ビルトインアセットが格納されているディレクトリへのパスを指定します。

指定しない場合は、Peridot CLIのひとつ上のディレクトリにあるbuiltin-assetsディレクトリが使用されます。
