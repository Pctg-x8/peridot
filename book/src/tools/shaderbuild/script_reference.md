# Combined Shader Script(csh) Syntax Reference

```bnf
toplevel_blocks ::= toplevel_block*
toplevel_block ::=
    | "VertexInput" ["{" binding_block* "}"]
    | "VertexShader" codeblock
    | "FragmentShader" codeblock
    | "Varyings" shader_stage "->" shader_stage "{" (ident,+ ":" glsl_represents_until_decl_end ";")* "}"
    | "SpecConstant" "[" shader_stage "]" "(" num ")" ident ":" glsl_represents_until("=") "=" glsl_represents_until_decl_end ";"
    | "Uniform" "[" shader_stage "]" "(" num "," num ")" ident codeblock
    | "SamplerBuffer" "[" shader_stage "]" "(" num "," num ")" ident
    | sampler "[" shader_stage "]" "(" num "," num ")" ident
    | "PushConstant" "[" shader_stage "]" ident codeblock
    | "Header" "[" shader_stage "]" codeblock
sampler ::= "Sampler1D" | "Sampler2D" | "Sampler3D"
shader_stage ::= "VertexShader" | "FragmentShader"

binding_block ::= "Binding" num ["[" ("PerInstance" | "PerVertex") "]"] "{" (ident,+ ":" glsl_represents_until_decl_end ";")* "}"
codeblock ::= "{" glsl_code "}"

ident ::= [a-zA-Z_][a-zA-Z0-9_]*
num ::= [0-9]+
glsl_represents_until_decl_end ::= [^;@]+
glsl_represents_until(c) ::= /* get characters until c */
glsl_code ::= /* read until block closes(nesting allowed) */
```

## VertexInput

このシェーダが処理できる頂点フォーマットを定義します。

## VertexShader / FragmentShader

頂点シェーダおよびフラグメントシェーダのmain関数内のコードを記述します。

おおよそGLSLそのままですが、次に示す出力変数が一部置き換わるようになっています。

* `RasterPosition`: VertexShaderでgl_Positionに置き換わる
* `Target[n]`: FragmentShaderで `layout(location = n)` がついたout変数に置き換わる

## Varyings

シェーダ間で渡される変数を定義します。
２つのシェーダステージを `->` で挟んで、前にあるステージではout変数として、後にあるステージではin変数として宣言されます。

## SpecConstant

特殊化定数（Specialization Constant）を宣言します。[]内で示したシェーダステージの、()で示した番号で宣言されるようになります。

## Uniform / SamplerBuffer / Sampler

各種リソースを定義します。()の中の数値はlayoutで指定されるもので、1つめがset、2つめがbindingです。

## PushConstant

Push Constantを宣言します。[]内で示したシェーダステージに宣言されます。
一つのシェーダステージに複数回定義することはできません。

## Header

mainコード以外の任意のGLSLコードを記述することができます。
PushConstantの次、mainの前に挿入されます。
