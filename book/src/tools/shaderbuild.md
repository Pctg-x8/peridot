# peridot-shaderbuild

**Combined Shader Script**を読み込み、Peridot Vertex Processing packファイルを生成します。

## Combined Shader Script

Vertex ShaderとFragment Shaderを同一のファイルに書くことでメンテナンスしやすくリンクミスの少ないシェーダを作れるようにしています。
UnityにおけるShaderLabと似たようなものです。

Combined Shader Scriptは単なるプリプロセッサ言語で、最終的にGLSLに変換された後glslcでコンパイルされます。
その後、shaderbuildではglslcの出力で得られたSPIR-Vバイナリをうまく結合してPeridot Vertex Processing packファイルを生成します。
