# Additional instructions for Android

Android向けにビルドする際に追加で設定する必要がある情報です。

## 必要なもの

### cargo-ndk

次のコマンドでインストールしてください。Peridot CLIは2.x系が前提となっています。

```sh
$ cargo install cargo-ndk
```

## 環境変数

### `NDK_HOME`

Android NDK r20のホーム（toolchainsなどのフォルダがある場所）を指定します。

### `NDK_PLATFORM_TARGET`

build.gradleのcompileSdkVersionに合わせてください。

### `ANDROID_HOME`

Android SDKのホームを指定してください。

エラーメッセージ `Task 'assembleDebug' not found in root project 'apkbuild'.` が出た場合は、この変数が設定されていない可能性があるので改めて設定することで解決する場合があります。  
[https://stackoverflow.com/questions/49187670/assembledebug-task-not-found-while-running-from-jenkins](https://stackoverflow.com/questions/49187670/assembledebug-task-not-found-while-running-from-jenkins)
