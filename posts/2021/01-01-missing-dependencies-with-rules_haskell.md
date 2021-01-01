---
title: rules_haskell でパッケージの依存関係がうまく解決できない時
tags: Haskell, Bazel
---

ちょっとした rules_haskell で起きたエラーに関するメモ書きです（Bazel の話）．

## 起きたエラー

具体的には「[MarkdownをMediumへポストするときにコードブロックをGistのリンクにする](/posts/2021-01-01-mdium-with-gist.html)」をやっていたときに起きたエラー：

```
$ bazel build //:mdium
...
ERROR: /.../external/stackage/BUILD.bazel:1277:22: HaskellCabalLibrary @stackage//:hslua failed (Exit 1) cabal_wrapper failed: error executing command bazel-out/host/bin/external/rules_haskell/haskell/cabal_wrapper lib:hslua hslua-1.3.0 true external/stackage/hslua-1.3.0/Setup.hs external/stackage/hslua-1.3.0 ... (remaining 9 argument(s) skipped)

Use --sandbox_debug to see verbose messages from the sandbox
Setup.hs: Encountered missing dependencies:
base-compat >=0.10
...
```

base-compat はバージョン 0.11.2 が入ってるはずなので，この依存関係は満たしているはずなのに...？？？

## 原因

hslua-1.3.0 の Cabal ファイルを読んでたら気づいた：

```cabal
  if impl(ghc < 8.8)
    build-depends:       base-compat          >= 0.10
    hs-source-dirs:      prelude
    other-modules:       Prelude
```

`if impl(ghc < 8.8)` の部分．
もしかして，Bazel の依存パッケージを解決してるときにこの分岐がうまくいってない？つまり GHC のバージョンが間違ってる？

正解でした．

## 対処

GHC のバージョンは rules_haskell の `rules_haskell_toolchains` の `version` 引数で指定できる．
指定しない場合は デフォルトで 8.6.5 が利用される．
決して，`stack_snapshot` で指定した LTS から自動で解決されない（はず）．
僕はこの仕様をすっかり忘れており，LTS では GHC 8.8 系を利用しているのに GHC 8.6 でビルドをしていた．
結果として，依存パッケージの解決がめちゃくちゃになっていたのだ．

対処法は簡単で，`rules_haskell_toolchains` で `version` を指定すれば良い：


```py
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-3b8182ca5287cf93687fff1cefd98910f683b679",
    urls = ["https://github.com/tweag/rules_haskell/archive/3b8182ca5287cf93687fff1cefd98910f683b679.tar.gz"],
    sha256 = "85f269adfecfc5760fae6017608f7efebfccb719c22c7e71af03c4887f54b08e",
)

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

rules_haskell_toolchains(version = "8.8.4")
```

ちなみに，rules_haskell の現在の最新のバージョンタグである v0.13 を使わずに，直接最新のコミットハッシュを指定しているのは，v0.13 では LTS で利用している 8.8.4 が無いからだ．

## おしまい

本当にちょっとしたことだけど念のため記事にしておいた．
rules_haskell が流行ったあかつきにはきっと助かる人が出るはず笑
