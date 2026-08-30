# Introduction: The Architecture

**Peridot Engine**（旧名称「Interlude 2」、以下**Peridot**）は、
マイクロカーネルアーキテクチャに発想を受けた、柔軟かつパワフルなゲームエンジンです。

このゲームエンジンは次に示す複数の層（レイヤー）から成っています。

![概略図](https://github.com/Pctg-x8/peridot/wiki/images/peridot_arch.png)

## BaseEngine

Peridotにおいて外すことの出来ないコアなモジュールとなります。
バックエンドシステムの起動や最低限の管理を担当しています。また、基本的なユーティリティ郡もここから提供されます。

## Module

必要に応じてopt-in可能な補助クレート郡です。拡張アセットの読み込みやバックエンドシステムが関係しない追加機能はこの形態で提供されます。

## MLE（Middle Layer Engine: 中位エンジン）

バックエンドシステム絡みやアセットロードなどよりひとつ上層に位置づけられ、アプリケーション処理の統括的な処理、管理の役割を持ちます。

## HLRE（High Layer Rendering Engine: 高位レンダリングエンジン）

MLEを使用するなどして実装される層で、その名の通り高度な描画処理の実行や管理を担当します。
HLRE自体はアプリケーションロジックを管理しません[^logic_manager]。

## UserCode

ユーザーが書くコード、またはMLE / HLREで処理可能なデータファイルとなります。

## Cradle

プラットフォームとPeridot Engine全体（BaseEngine以上）の橋渡しを担当します。

[^logic_manager]: MLE / UserCodeの役割になります。
